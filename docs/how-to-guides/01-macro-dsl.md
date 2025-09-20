# CHIP-8マクロDSL実装ガイド

## 概要

このガイドでは、Common Lispのマクロシステムを活用してCHIP-8命令セットを実装するためのDSL（Domain Specific Language）構築手法を解説します。基本マクロから段階的に発展させ、保守性・可読性・性能を向上させた実装を目指します。

> **関連ガイド**: メタプログラミング技術については [04-metaprogramming.md](04-metaprogramming.md) を参照してください。

## 段階的マクロ実装アプローチ

### 実装方針

本ガイドでは3段階のアプローチでCHIP-8命令実装DSLを構築します：

1. **基本DSL**: 冗長性を排除した基本的な命令定義マクロ
2. **コンテキスト対応DSL**: 実行環境を自動管理する改良版
3. **宣言的DSL**: メタデータと最適化ヒントを含む宣言的定義

### 各段階の特徴

| 段階 | 主要機能 | 用途 |
|------|----------|------|
| 基本DSL | 命令定義の冗長性排除 | プロトタイピング・学習 |
| コンテキスト対応 | 自動的なCPU状態管理 | 実用的な開発 |
| 宣言的 | メタデータ・最適化統合 | 本格的なプロダクション |

## 基本DSL: 命令定義マクロ

最初の段階では、基本的なマクロを使って命令定義の冗長性を排除します。

```lisp
(defpackage #:chip8-basic-dsl
  (:use #:common-lisp)
  (:export #:definstruction #:operand #:advance-pc))

(in-package #:chip8-basic-dsl)

;; 基本的な命令定義マクロ
(defmacro definstruction (name pattern description &body body)
  "CHIP-8命令の基本定義マクロ"
  (let ((func-name (intern (format nil "EXECUTE-~A" name)))
        (opcode-var (gensym "OPCODE"))
        (cpu-var (gensym "CPU"))
        (memory-var (gensym "MEMORY"))
        (display-var (gensym "DISPLAY")))

    `(progn
       (defun ,func-name (,cpu-var ,memory-var ,display-var ,opcode-var)
         ,description
         (declare (type cpu ,cpu-var)
                  (type memory-manager ,memory-var)
                  (type display-system ,display-var)
                  (type (unsigned-byte 16) ,opcode-var)
                  (optimize (speed 3) (safety 1)))

         ;; オペランド抽出
         (let ,(extract-operands pattern opcode-var)
           ,@body))

       ;; 命令テーブルに登録
       (register-instruction ',pattern #',func-name))))

;; オペランド抽出ヘルパー
(defun extract-operands (pattern opcode-var)
  "オペコードパターンからオペランド抽出コードを生成"
  (let ((bindings '()))
    (when (find #\X pattern)
      (push `(x (ldb (byte 4 8) ,opcode-var)) bindings))
    (when (find #\Y pattern)
      (push `(y (ldb (byte 4 4) ,opcode-var)) bindings))
    (when (find #\N pattern)
      (push `(n (ldb (byte 4 0) ,opcode-var)) bindings))

    ;; 複合オペランド
    (cond
      ((search "NNN" pattern)
       (push `(nnn (ldb (byte 12 0) ,opcode-var)) bindings))
      ((search "NN" pattern)
       (push `(nn (ldb (byte 8 0) ,opcode-var)) bindings)))

    (nreverse bindings)))

;; ヘルパー関数
(defun advance-pc (cpu)
  "プログラムカウンタを2進める"
  (incf (cpu-program-counter cpu) 2))
```

### 基本DSLでの命令実装例

```lisp
;; システム命令群
(definstruction cls "00E0" "画面をクリアする"
  (clear-display display)
  (advance-pc cpu))

(definstruction ret "00EE" "サブルーチンから復帰"
  (when (zerop (cpu-stack-pointer cpu))
    (error "Stack underflow"))
  (decf (cpu-stack-pointer cpu))
  (setf (cpu-program-counter cpu)
        (aref (cpu-stack cpu) (cpu-stack-pointer cpu))))

;; レジスタ操作命令群
(definstruction ld-vx-byte "6XNN" "VXに値を設定"
  (setf (aref (cpu-registers cpu) x) nn)
  (advance-pc cpu))

(definstruction add-vx-byte "7XNN" "VXに値を加算"
  (setf (aref (cpu-registers cpu) x)
        (mod (+ (aref (cpu-registers cpu) x) nn) 256))
  (advance-pc cpu))

;; ALU命令群
(definstruction add-vx-vy "8XY4" "VX += VY (キャリーフラグ付き)"
  (let ((result (+ (aref (cpu-registers cpu) x)
                   (aref (cpu-registers cpu) y))))
    (setf (aref (cpu-registers cpu) #xF) (if (> result 255) 1 0))
    (setf (aref (cpu-registers cpu) x) (mod result 256)))
  (advance-pc cpu))
```

## コンテキスト対応DSL

第2段階では、CPUコンテキストを自動的に管理し、より自然な記述を可能にします。

```lisp
(defpackage #:chip8-context-dsl
  (:use #:common-lisp #:chip8-basic-dsl)
  (:export #:with-cpu-context #:definstruction-v2 #:reg #:set-reg #:skip #:jump-to))

(in-package #:chip8-context-dsl)

;; CPUコンテキスト管理マクロ
(defmacro with-cpu-context ((cpu-var memory-var display-var input-var) &body body)
  "CPU実行コンテキストを提供"
  `(let ((*current-cpu* ,cpu-var)
         (*current-memory* ,memory-var)
         (*current-display* ,display-var)
         (*current-input* ,input-var))
     (symbol-macrolet ((pc (cpu-program-counter *current-cpu*))
                       (sp (cpu-stack-pointer *current-cpu*))
                       (i-reg (cpu-index-register *current-cpu*))
                       (dt (cpu-delay-timer *current-cpu*))
                       (st (cpu-sound-timer *current-cpu*))
                       (stack (cpu-stack *current-cpu*))
                       (registers (cpu-registers *current-cpu*)))
       ,@body)))

;; 改良された命令定義マクロ
(defmacro definstruction-v2 (name pattern description &body body)
  "コンテキスト対応命令定義マクロ"
  (let ((func-name (intern (format nil "EXECUTE-~A" name)))
        (opcode-var (gensym "OPCODE")))

    `(progn
       (defun ,func-name (cpu memory display input ,opcode-var)
         ,description
         (declare (optimize (speed 3) (safety 1)))

         (with-cpu-context (cpu memory display input)
           (let ,(extract-operands pattern opcode-var)
             ,@body
             ;; 自動PC進行（ジャンプ系以外）
             ,(unless (jump-instruction-p name)
                `(incf pc 2)))))

       (register-instruction ',pattern #',func-name))))

;; 便利なヘルパーマクロ
(defmacro reg (index)
  "レジスタアクセス"
  `(aref registers ,index))

(defmacro set-reg (index value)
  "レジスタ設定"
  `(setf (aref registers ,index) ,value))

(defmacro skip ()
  "次の命令をスキップ"
  `(incf pc 2))

(defmacro jump-to (address)
  "指定アドレスにジャンプ"
  `(setf pc ,address))

;; ジャンプ命令判定
(defun jump-instruction-p (name)
  "ジャンプ系命令かどうか判定"
  (member name '(jp call ret jp-v0-addr ld-vx-k)))
```

### コンテキスト対応DSLでの実装例

```lisp
;; より自然な記述
(definstruction-v2 cls-v2 "00E0" "画面をクリアする"
  (clear-display *current-display*))

(definstruction-v2 se-vx-byte-v2 "3XNN" "VX == NN でスキップ"
  (when (= (reg x) nn)
    (skip)))

(definstruction-v2 ld-vx-byte-v2 "6XNN" "VXに値を設定"
  (set-reg x nn))

(definstruction-v2 add-vx-vy-v2 "8XY4" "VX += VY (キャリーフラグ付き)"
  (let ((result (+ (reg x) (reg y))))
    (set-reg #xF (if (> result 255) 1 0))
    (set-reg x (mod result 256))))
```

## 宣言的命令仕様DSL

第3段階では、命令の動作を宣言的に記述し、メタデータも含めて管理します。

```lisp
(defpackage #:chip8-declarative-dsl
  (:use #:common-lisp)
  (:export #:defchip8-instruction #:defalu-family #:instruction-spec))

(in-package #:chip8-declarative-dsl)

;; 宣言的命令定義マクロ
(defmacro defchip8-instruction (name &key pattern description
                                     reads writes flags timing
                                     side-effects implementation)
  "宣言的なCHIP-8命令定義"
  `(progn
     ;; メタデータの記録
     (setf (get ',name :instruction-spec)
           (list :pattern ',pattern
                 :description ',description
                 :reads ',reads
                 :writes ',writes
                 :flags ',flags
                 :timing ',timing
                 :side-effects ',side-effects))

     ;; 実装の生成
     ,(generate-instruction-implementation name pattern implementation)

     ;; 最適化ヒントの生成
     ,(generate-optimization-hints name reads writes side-effects)

     ;; テストケースの生成
     ,(generate-test-cases name pattern reads writes flags)))

;; ALU命令ファミリー定義マクロ
(defmacro defalu-family ()
  "ALU命令群を一括定義"
  `(progn
     ,@(mapcar #'generate-alu-instruction *alu-instruction-specs*)))

(defparameter *alu-instruction-specs*
  '((:set    "8XY0" "VX = VY"           (vy) (vx) nil nil)
    (:or     "8XY1" "VX = VX | VY"      (vx vy) (vx) ((vf :cleared)) nil)
    (:and    "8XY2" "VX = VX & VY"      (vx vy) (vx) ((vf :cleared)) nil)
    (:xor    "8XY3" "VX = VX ^ VY"      (vx vy) (vx) ((vf :cleared)) nil)
    (:add    "8XY4" "VX = VX + VY"      (vx vy) (vx vf) ((vf :carry)) nil)
    (:sub    "8XY5" "VX = VX - VY"      (vx vy) (vx vf) ((vf :not-borrow)) nil)
    (:shr    "8XY6" "VX = VX >> 1"      (vx) (vx vf) ((vf :shifted-bit)) nil)
    (:subn   "8XY7" "VX = VY - VX"      (vx vy) (vx vf) ((vf :not-borrow)) nil)
    (:shl    "8XYE" "VX = VX << 1"      (vx) (vx vf) ((vf :shifted-bit)) nil)))
```

### 宣言的DSLでの実装例

```lisp
;; システム命令群
(defchip8-instruction cls
  :pattern "00E0"
  :description "画面をクリアする"
  :reads ()
  :writes ()
  :flags ()
  :side-effects (:display-clear)
  :implementation
  ((clear-display *current-display*)))

;; レジスタ操作命令群
(defchip8-instruction ld-vx-byte
  :pattern "6XNN"
  :description "VXに値を設定"
  :reads (nn)
  :writes (vx)
  :flags ()
  :side-effects ()
  :implementation
  ((set-reg x nn)))

;; 表示命令
(defchip8-instruction drw-vx-vy-n
  :pattern "DXYN"
  :description "スプライト描画"
  :reads (vx vy n i-reg memory)
  :writes (vf display)
  :flags ((vf :collision))
  :side-effects (:display-modify :memory-read)
  :implementation
  ((let ((x-pos (reg x))
         (y-pos (reg y))
         (collision 0))
     (loop for row from 0 below n
           for y from y-pos
           do (let ((sprite-byte (read-memory *current-memory* (+ i-reg row))))
                (loop for bit from 7 downto 0
                      for x from x-pos
                      when (logbitp bit sprite-byte)
                      do (when (flip-pixel *current-display* (mod x 64) (mod y 32))
                           (setf collision 1)))))
     (set-reg #xF collision))))

;; ALU命令群を一括生成
(defalu-family)
```

## 実用的な応用例

### プロダクション品質の命令実装

宣言的DSLを使用した実際の命令実装例：

```lisp
;; メタデータから自動生成されるテストケース
(deftest test-add-vx-vy
  "ADD VX, VY命令のテスト"
  (let ((cpu (make-test-cpu))
        (memory (make-test-memory))
        (display (make-test-display)))

    ;; 通常加算のテスト
    (setf (reg 0) 10 (reg 1) 20)
    (execute-add-vx-vy cpu memory display #x8014)
    (is (= (reg 0) 30))
    (is (= (reg #xF) 0))

    ;; オーバーフローのテスト
    (setf (reg 0) 200 (reg 1) 100)
    (execute-add-vx-vy cpu memory display #x8014)
    (is (= (reg 0) 44))  ; (200 + 100) mod 256
    (is (= (reg #xF) 1)))) ; キャリーフラグ

;; 最適化ヒントから生成される特化版
(defun execute-add-vx-vy-optimized (cpu memory display opcode)
  "最適化されたADD VX, VY実装"
  (declare (optimize (speed 3) (safety 0))
           (type cpu cpu)
           (type (unsigned-byte 16) opcode))

  (let ((x (ldb (byte 4 8) opcode))
        (y (ldb (byte 4 4) opcode)))
    (declare (type (unsigned-byte 4) x y))

    (let ((vx (the (unsigned-byte 8) (aref (cpu-registers cpu) x)))
          (vy (the (unsigned-byte 8) (aref (cpu-registers cpu) y))))
      (declare (type (unsigned-byte 8) vx vy))

      (let ((result (the (unsigned-byte 9) (+ vx vy))))
        (setf (aref (cpu-registers cpu) #xF)
              (if (> result 255) 1 0))
        (setf (aref (cpu-registers cpu) x)
              (the (unsigned-byte 8) (logand result 255)))))))
```

### 追加機能

本ガイドで紹介した3段階のDSLに加えて、以下の機能があります：

#### 型安全性とコンパイル時最適化

```lisp
;; 型安全な命令定義の例
(deftyped-instruction add-with-types "8XY4"
  "型安全な加算命令"
  ((x register-index) (y register-index))
  (let ((result (the (unsigned-byte 9) (+ (reg x) (reg y)))))
    (set-reg #xF (if (> result 255) 1 0))
    (set-reg x (the byte-value (mod result 256)))))
```

#### リーダーマクロによる構文拡張

```lisp
;; 自然言語風の記述
(definstruction add-natural "8XY4"
  "自然な加算記述"
  (let ((sum (+ V[x] V[y])))
    #I IF (> sum 255) THEN (set-reg #xF 1)
    (set-reg x (mod sum 256))))
```

#### メタDSLシステム

メタプログラミングシステムの構築については、[04-metaprogramming.md](04-metaprogramming.md)で解説されています。

```lisp
;; DSL自体を生成するメタDSLの例
(define-instruction-language chip8-extended
  :syntax-extensions
  (;; 自然言語風の構文
   (when-condition "IF ~A THEN ~A")
   (register-syntax "V[~A]"))

  :optimization-rules
  (;; 頻度ベース最適化
   (inline-hot-instructions :threshold 1000)
   (constant-folding :aggressive t)))
```

## 結論

本ガイドで紹介した段階的マクロDSLアプローチにより、CHIP-8命令セットの実装において以下の利点が得られます：

### 実装上の利点

1. **保守性**: 仕様変更が局所的に収まり、影響範囲を最小化
2. **可読性**: 命令の意図がコードから直接理解可能
3. **再利用性**: 共通パターンをマクロで抽象化し、重複を排除
4. **一貫性**: 統一された記述方法による品質の向上

### 開発効率の向上

- **迅速なプロトタイピング**: 基本DSLでの素早い実装
- **段階的改善**: コンテキスト対応・宣言的DSLへの発展
- **自動テスト生成**: メタデータからのテストケース自動生成
- **ドキュメント統合**: 実装とドキュメントの一体化

### 次のステップ

- **メタプログラミング**: [04-metaprogramming.md](04-metaprogramming.md)で技術を学習
- **パフォーマンス最適化**: [02-performance-optimization.md](02-performance-optimization.md)で性能向上手法を習得
- **テスト手法**: [03-property-testing.md](03-property-testing.md)で検証手法を理解

Common Lispのマクロシステムを活用することで、CHIP-8エミュレーター開発は単なるコーディング作業から、ドメイン固有言語の設計・実装という創造的な活動に変わります。

## 参考実装

本ガイドで紹介したマクロDSLの実装例は、プロジェクトの`src/dsl/`ディレクトリに段階的に配置されています。各段階の動作確認とベンチマークが可能な形で提供されており、実際のプロジェクトでの採用を容易にします。