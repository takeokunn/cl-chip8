# マクロシステムを活用した命令実装

## 概要

このガイドでは、Common Lispの強力なマクロシステムを活用して、CHIP-8の命令セットを効率的かつ可読性高く実装する方法を説明します。

## 問題の特定

CHIP-8には35の命令があり、それぞれ異なるオペランドとロジックを持ちます。従来のアプローチでは：

- **冗長なコード**: 似たような処理が繰り返される
- **エラーが起きやすい**: 手動でのコード複製によるバグ
- **保守性の低下**: 仕様変更時の影響範囲が大きい
- **可読性の問題**: 命令の意図が見えにくい

## マクロによる解決策

### 1. 基本的な命令定義マクロ

```lisp
(defmacro define-chip8-instruction (name opcode-pattern description &body body)
  "CHIP-8命令を定義するマクロ"
  (let ((instruction-function (intern (format nil "EXECUTE-~A" name)))
        (opcode-var (gensym "OPCODE"))
        (cpu-var (gensym "CPU"))
        (memory-var (gensym "MEMORY"))
        (display-var (gensym "DISPLAY")))

    `(progn
       (defun ,instruction-function (,cpu-var ,memory-var ,display-var ,opcode-var)
         ,description
         (declare (type cpu ,cpu-var)
                 (type memory-manager ,memory-var)
                 (type display-system ,display-var)
                 (type word-value ,opcode-var)
                 (optimize (speed 3) (safety 1)))

         ;; オペコードからオペランド抽出
         (let ,(extract-operands opcode-pattern opcode-var)
           ,@body
           ;; プログラムカウンタを自動的に進める（ジャンプ系を除く）
           ,(unless (member name '(jump call return))
              `(incf (cpu-program-counter ,cpu-var) 2))))

       ;; 命令テーブルに登録
       (register-instruction ,opcode-pattern #',instruction-function))))

(defun extract-operands (pattern opcode-var)
  "オペコードパターンからオペランド抽出コードを生成"
  (let ((bindings '()))
    (when (find #\X pattern)
      (push `(x (ldb (byte 4 8) ,opcode-var)) bindings))
    (when (find #\Y pattern)
      (push `(y (ldb (byte 4 4) ,opcode-var)) bindings))
    (when (find-if (lambda (c) (char= c #\N)) pattern)
      (let ((n-count (count #\N pattern)))
        (case n-count
          (1 (push `(n (ldb (byte 4 0) ,opcode-var)) bindings))
          (2 (push `(nn (ldb (byte 8 0) ,opcode-var)) bindings))
          (3 (push `(nnn (ldb (byte 12 0) ,opcode-var)) bindings)))))
    (nreverse bindings)))
```

### 2. 実際の命令実装例

```lisp
;; システム命令
(define-chip8-instruction clear-screen "00E0"
  "画面をクリアする"
  (clear-display display))

(define-chip8-instruction return-from-subroutine "00EE"
  "サブルーチンから復帰"
  (when (zerop (cpu-stack-pointer cpu))
    (error "Stack underflow"))
  (decf (cpu-stack-pointer cpu))
  (setf (cpu-program-counter cpu)
        (aref (cpu-stack cpu) (cpu-stack-pointer cpu))))

;; ジャンプ・分岐命令
(define-chip8-instruction jump "1NNN"
  "指定アドレスにジャンプ"
  (setf (cpu-program-counter cpu) nnn))

(define-chip8-instruction call-subroutine "2NNN"
  "サブルーチンを呼び出す"
  (when (>= (cpu-stack-pointer cpu) 16)
    (error "Stack overflow"))
  (setf (aref (cpu-stack cpu) (cpu-stack-pointer cpu))
        (cpu-program-counter cpu))
  (incf (cpu-stack-pointer cpu))
  (setf (cpu-program-counter cpu) nnn))

;; 条件分岐
(define-chip8-instruction skip-if-equal "3XNN"
  "VXがNNと等しい場合、次の命令をスキップ"
  (when (= (aref (cpu-registers cpu) x) nn)
    (incf (cpu-program-counter cpu) 2)))

(define-chip8-instruction skip-if-not-equal "4XNN"
  "VXがNNと等しくない場合、次の命令をスキップ"
  (when (/= (aref (cpu-registers cpu) x) nn)
    (incf (cpu-program-counter cpu) 2)))

;; レジスタ操作
(define-chip8-instruction set-register "6XNN"
  "VXにNNを設定"
  (setf (aref (cpu-registers cpu) x) nn))

(define-chip8-instruction add-to-register "7XNN"
  "VXにNNを加算"
  (setf (aref (cpu-registers cpu) x)
        (mod (+ (aref (cpu-registers cpu) x) nn) 256)))
```

### 3. ALU命令の高度なマクロ

```lisp
(defmacro define-alu-instruction (name operation description)
  "ALU命令を定義する高度なマクロ"
  `(define-chip8-instruction ,name ,(format nil "8XY~X" (position name *alu-operations*))
     ,description
     (let ((vx (aref (cpu-registers cpu) x))
           (vy (aref (cpu-registers cpu) y)))
       ,(ecase operation
          (:set
           `(setf (aref (cpu-registers cpu) x) vy))
          (:or
           `(setf (aref (cpu-registers cpu) x) (logior vx vy)))
          (:and
           `(setf (aref (cpu-registers cpu) x) (logand vx vy)))
          (:xor
           `(setf (aref (cpu-registers cpu) x) (logxor vx vy)))
          (:add
           `(let ((result (+ vx vy)))
              (setf (aref (cpu-registers cpu) #xF) (if (> result 255) 1 0))
              (setf (aref (cpu-registers cpu) x) (mod result 256))))
          (:sub
           `(progn
              (setf (aref (cpu-registers cpu) #xF) (if (>= vx vy) 1 0))
              (setf (aref (cpu-registers cpu) x) (mod (- vx vy) 256))))
          (:shr
           `(progn
              (setf (aref (cpu-registers cpu) #xF) (logand vx 1))
              (setf (aref (cpu-registers cpu) x) (ash vx -1))))
          (:subn
           `(progn
              (setf (aref (cpu-registers cpu) #xF) (if (>= vy vx) 1 0))
              (setf (aref (cpu-registers cpu) x) (mod (- vy vx) 256))))
          (:shl
           `(progn
              (setf (aref (cpu-registers cpu) #xF) (if (> vx 127) 1 0))
              (setf (aref (cpu-registers cpu) x) (mod (ash vx 1) 256))))))))

(defparameter *alu-operations*
  '(set-register-to-register or-registers and-registers xor-registers
    add-registers subtract-registers shift-right subtract-from
    shift-left))

;; ALU命令の定義
(define-alu-instruction set-register-to-register :set
  "VXにVYの値を設定")

(define-alu-instruction or-registers :or
  "VXとVYの論理和をVXに設定")

(define-alu-instruction and-registers :and
  "VXとVYの論理積をVXに設定")

(define-alu-instruction add-registers :add
  "VXにVYを加算、キャリーフラグを設定")

(define-alu-instruction subtract-registers :sub
  "VXからVYを減算、ボローフラグを設定")
```

### 4. 命令テーブル管理

```lisp
(defparameter *instruction-table* (make-hash-table :test 'equal)
  "命令実行関数のテーブル")

(defun register-instruction (pattern function)
  "命令をテーブルに登録"
  (setf (gethash pattern *instruction-table*) function))

(defun find-instruction (opcode)
  "オペコードに対応する命令を検索"
  (declare (type word-value opcode)
           (optimize (speed 3) (safety 1)))

  (maphash (lambda (pattern function)
             (when (match-opcode-pattern pattern opcode)
               (return-from find-instruction function)))
           *instruction-table*)
  nil)

(defun match-opcode-pattern (pattern opcode)
  "オペコードがパターンにマッチするかチェック"
  (declare (type string pattern)
           (type word-value opcode))

  (let ((pattern-bits (parse-pattern pattern))
        (opcode-bits (format nil "~4,'0X" opcode)))
    (loop for p across pattern-bits
          for o across opcode-bits
          always (or (char= p o)
                    (find p "XYN")))))
```

### 5. デバッグとロギング機能

```lisp
(defmacro with-instruction-logging (cpu &body body)
  "命令実行ログを記録"
  (let ((start-time (gensym "START"))
        (end-time (gensym "END"))
        (result (gensym "RESULT")))
    `(let ((,start-time (get-internal-real-time)))
       (when *debug-mode*
         (format *debug-output* "Executing instruction at PC: ~4,'0X~%"
                (cpu-program-counter ,cpu)))

       (let ((,result (progn ,@body)))
         (let ((,end-time (get-internal-real-time)))
           (when *debug-mode*
             (format *debug-output* "Execution time: ~A ms~%"
                    (/ (- ,end-time ,start-time)
                       (/ internal-time-units-per-second 1000)))))
         ,result))))

(defmacro define-traced-instruction (name opcode-pattern description &body body)
  "トレース機能付きの命令定義"
  `(define-chip8-instruction ,name ,opcode-pattern ,description
     (with-instruction-logging cpu
       ,@body)))
```

### 6. 条件コンパイル

```lisp
(defmacro define-optimized-instruction (name opcode-pattern description
                                       &key debug-body release-body)
  "デバッグ・リリース版で異なる実装を持つ命令"
  `(define-chip8-instruction ,name ,opcode-pattern ,description
     #+debug-mode
     (progn ,@debug-body)
     #-debug-mode
     (progn ,@release-body)))

;; 使用例
(define-optimized-instruction optimized-add "7XNN"
  "最適化されたレジスタ加算"
  :debug-body ((format t "Adding ~A to register V~X~%" nn x)
               (setf (aref (cpu-registers cpu) x)
                     (mod (+ (aref (cpu-registers cpu) x) nn) 256)))
  :release-body ((setf (aref (cpu-registers cpu) x)
                       (the byte-value
                            (mod (+ (aref (cpu-registers cpu) x) nn) 256)))))
```

### 7. Property-Based Testing対応

```lisp
(defmacro define-testable-instruction (name opcode-pattern description
                                      &key properties body)
  "Property-Based Testing対応の命令定義"
  `(progn
     (define-chip8-instruction ,name ,opcode-pattern ,description
       ,@body)

     ;; プロパティベーステストの生成
     ,@(mapcar (lambda (prop)
                 `(defproperty ,(intern (format nil "PROP-~A-~A" name (first prop)))
                    ,(second prop)
                    ,(third prop)))
               properties)))

;; 使用例
(define-testable-instruction testable-add "7XNN"
  "テスト可能なレジスタ加算"
  :properties ((no-overflow (reg val)
                           (let ((cpu (make-test-cpu)))
                             (setf (aref (cpu-registers cpu) reg) 100)
                             (execute-testable-add cpu nil nil (logior #x7000 (ash reg 8) val))
                             (<= (aref (cpu-registers cpu) reg) 255)))
               (commutative (a b)
                           (let ((cpu1 (make-test-cpu))
                                 (cpu2 (make-test-cpu)))
                             (setf (aref (cpu-registers cpu1) 0) a)
                             (setf (aref (cpu-registers cpu2) 0) a)
                             (execute-testable-add cpu1 nil nil (logior #x7000 b))
                             (execute-testable-add cpu2 nil nil (logior #x7000 b))
                             (= (aref (cpu-registers cpu1) 0)
                                (aref (cpu-registers cpu2) 0)))))
  :body (setf (aref (cpu-registers cpu) x)
              (mod (+ (aref (cpu-registers cpu) x) nn) 256)))
```

## 実践例

### 完全な命令セットの実装

```lisp
;; システム命令群
(define-chip8-instruction cls "00E0" "画面クリア"
  (clear-display display))

(define-chip8-instruction ret "00EE" "リターン"
  (pop-from-stack cpu))

;; フロー制御命令群
(define-chip8-instruction jp "1NNN" "ジャンプ"
  (setf (cpu-program-counter cpu) nnn))

(define-chip8-instruction call "2NNN" "サブルーチン呼び出し"
  (push-to-stack cpu (cpu-program-counter cpu))
  (setf (cpu-program-counter cpu) nnn))

;; 条件分岐命令群
(define-chip8-instruction se-vx-byte "3XNN" "VX == NN でスキップ"
  (when (= (reg cpu x) nn)
    (skip-instruction cpu)))

(define-chip8-instruction sne-vx-byte "4XNN" "VX != NN でスキップ"
  (when (/= (reg cpu x) nn)
    (skip-instruction cpu)))

;; ヘルパー関数
(defun reg (cpu index)
  "レジスタアクセスのヘルパー"
  (aref (cpu-registers cpu) index))

(defun (setf reg) (value cpu index)
  "レジスタ設定のヘルパー"
  (setf (aref (cpu-registers cpu) index) value))

(defun skip-instruction (cpu)
  "次の命令をスキップ"
  (incf (cpu-program-counter cpu) 2))
```

## メリット

この方法により：

1. **保守性向上**: 仕様変更が一箇所の変更で済む
2. **エラー削減**: 自動生成によりタイポなどのエラーを防止
3. **可読性向上**: 命令の意図が明確に表現される
4. **拡張性**: 新しい機能（ログ、テストなど）を容易に追加
5. **パフォーマンス**: コンパイル時最適化により高速実行

`★ Insight ─────────────────────────────────────`
Common Lispのマクロシステムは、コンパイル時にコード生成を行うため、実行時のオーバーヘッドがありません。このアプローチにより、DSL（Domain Specific Language）を構築し、CHIP-8の命令セットを自然な形で表現できます。また、マクロによる抽象化により、デバッグ機能やテスト機能を統一的に組み込むことができ、保守性の高いコードが実現できます。
`─────────────────────────────────────────────────`