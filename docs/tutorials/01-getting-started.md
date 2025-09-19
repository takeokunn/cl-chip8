# はじめに: CHIP-8エミュレーターの基本構築

## このチュートリアルについて

このチュートリアルでは、Common Lispを使用してCHIP-8エミュレーターを段階的に構築していきます。完全な初心者でも理解できるよう、基本的な概念から始めて、最終的に動作するエミュレーターを完成させます。

## 前提条件

- Common Lispの基本的な知識
- SBCL（Steel Bank Common Lisp）のインストール
- Quicklisp のセットアップ
- テキストエディタまたはIDE（Emacs + SLIME推奨）

## プロジェクトセットアップ

### 1. プロジェクト構造の作成

まず、プロジェクトのディレクトリ構造を作成します：

```
cl-chip8/
├── cl-chip8.asd           ; システム定義ファイル
├── src/                   ; ソースコード
│   ├── package.lisp       ; パッケージ定義
│   ├── cpu.lisp          ; CPU実装
│   ├── memory.lisp       ; メモリ管理
│   ├── display.lisp      ; 表示システム
│   ├── input.lisp        ; 入力処理
│   ├── audio.lisp        ; 音声システム
│   └── emulator.lisp     ; メインエミュレーターファサード
├── tests/                 ; テストコード
├── roms/                  ; テスト用ROMファイル
└── docs/                  ; ドキュメント
```

### 2. システム定義ファイル（cl-chip8.asd）

```lisp
(defsystem "cl-chip8"
  :description "World-class CHIP-8 emulator in Common Lisp"
  :author "あなたの名前"
  :license "MIT"
  :version "0.1.0"
  :depends-on ()  ; 外部依存を最小限に抑制
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "memory" :depends-on ("package"))
                 (:file "cpu" :depends-on ("package" "memory"))
                 (:file "display" :depends-on ("package"))
                 (:file "input" :depends-on ("package"))
                 (:file "audio" :depends-on ("package"))
                 (:file "emulator" :depends-on ("package" "cpu" "memory" "display" "input" "audio")))))
  :in-order-to ((test-op (test-op "cl-chip8/tests"))))

(defsystem "cl-chip8/tests"
  :description "Test system for cl-chip8"
  :depends-on ("cl-chip8" "fiveam")
  :components ((:module "tests"
                :components
                ((:file "package")
                 (:file "test-memory" :depends-on ("package"))
                 (:file "test-cpu" :depends-on ("package"))
                 (:file "test-integration" :depends-on ("package")))))
  :perform (test-op (op c) (symbol-call :fiveam :run-all-tests)))
```

### 3. パッケージ定義（src/package.lisp）

```lisp
(defpackage #:cl-chip8
  (:use #:common-lisp)
  (:nicknames #:chip8)
  (:export
   ;; エミュレーターメインクラス
   #:chip8-emulator
   #:make-emulator
   #:load-rom
   #:run-emulator
   #:step-emulator
   #:reset-emulator

   ;; CPU関連
   #:cpu
   #:cpu-registers
   #:cpu-program-counter
   #:cpu-stack-pointer
   #:cpu-index-register

   ;; メモリ関連
   #:memory-manager
   #:read-memory
   #:write-memory
   #:load-font-data

   ;; 表示関連
   #:display-system
   #:clear-display
   #:draw-sprite
   #:get-pixel

   ;; 入力関連
   #:input-handler
   #:key-pressed-p
   #:wait-for-key

   ;; 音声関連
   #:audio-system
   #:beep

   ;; デバッグ関連
   #:debugger
   #:set-breakpoint
   #:step-debug
   #:dump-state))

;; テストパッケージ
(defpackage #:cl-chip8-tests
  (:use #:common-lisp #:cl-chip8 #:fiveam)
  (:export #:run-all-tests))
```

## 基本データ構造の定義

### 型定義

```lisp
;; 基本的な型定義
(deftype byte-value () '(unsigned-byte 8))
(deftype word-value () '(unsigned-byte 16))
(deftype address () '(unsigned-byte 16))
(deftype register-index () '(integer 0 15))

;; メモリとレジスタの型
(deftype chip8-memory () '(simple-array byte-value (4096)))
(deftype chip8-registers () '(simple-array byte-value (16)))
(deftype chip8-stack () '(simple-array word-value (16)))
(deftype chip8-display () '(simple-array bit (64 32)))
```

### CPUクラスの基本定義

```lisp
(defclass cpu ()
  ((registers :initform (make-array 16 :element-type 'byte-value :initial-element 0)
             :accessor cpu-registers
             :type chip8-registers
             :documentation "16個の汎用レジスタ（V0-VF）")

   (index-register :initform 0
                  :accessor cpu-index-register
                  :type word-value
                  :documentation "アドレス用Iレジスタ")

   (program-counter :initform #x200
                   :accessor cpu-program-counter
                   :type address
                   :documentation "プログラムカウンター")

   (stack-pointer :initform 0
                 :accessor cpu-stack-pointer
                 :type byte-value
                 :documentation "スタックポインター")

   (stack :initform (make-array 16 :element-type 'word-value :initial-element 0)
         :accessor cpu-stack
         :type chip8-stack
         :documentation "サブルーチンスタック")

   (delay-timer :initform 0
               :accessor cpu-delay-timer
               :type byte-value
               :documentation "遅延タイマー")

   (sound-timer :initform 0
               :accessor cpu-sound-timer
               :type byte-value
               :documentation "音声タイマー"))
  (:documentation "CHIP-8 CPU状態を管理するクラス"))
```

## 最初の命令実装

### 命令デコード

```lisp
(defun decode-instruction (opcode)
  "16ビットオペコードを解析して命令情報を返す"
  (declare (type word-value opcode)
           (optimize (speed 3) (safety 1)))

  (let ((nibble1 (ldb (byte 4 12) opcode))  ; 上位4ビット
        (nibble2 (ldb (byte 4 8) opcode))   ; 次の4ビット
        (nibble3 (ldb (byte 4 4) opcode))   ; 次の4ビット
        (nibble4 (ldb (byte 4 0) opcode))   ; 下位4ビット
        (nnn (ldb (byte 12 0) opcode))      ; 下位12ビット
        (nn (ldb (byte 8 0) opcode))        ; 下位8ビット
        (x (ldb (byte 4 8) opcode))         ; X レジスタ
        (y (ldb (byte 4 4) opcode)))        ; Y レジスタ

    (case nibble1
      (#x0 (case nn
             (#xE0 :clear-screen)
             (#xEE :return)
             (t :sys-call)))
      (#x1 (list :jump nnn))
      (#x2 (list :call nnn))
      (#x3 (list :skip-if-eq x nn))
      (#x4 (list :skip-if-neq x nn))
      (#x5 (list :skip-if-reg-eq x y))
      (#x6 (list :set-reg x nn))
      (#x7 (list :add-reg x nn))
      (#x8 (case nibble4
             (#x0 (list :set-reg-reg x y))
             (#x1 (list :or-reg x y))
             (#x2 (list :and-reg x y))
             (#x3 (list :xor-reg x y))
             (#x4 (list :add-reg-reg x y))
             (#x5 (list :sub-reg x y))
             (#x6 (list :shr-reg x))
             (#x7 (list :subn-reg x y))
             (#xE (list :shl-reg x))))
      (#x9 (list :skip-if-reg-neq x y))
      (#xA (list :set-index nnn))
      (#xB (list :jump-plus-v0 nnn))
      (#xC (list :random x nn))
      (#xD (list :draw x y nibble4))
      (#xE (case nn
             (#x9E (list :skip-if-key x))
             (#xA1 (list :skip-if-not-key x))))
      (#xF (case nn
             (#x07 (list :get-delay x))
             (#x0A (list :wait-key x))
             (#x15 (list :set-delay x))
             (#x18 (list :set-sound x))
             (#x1E (list :add-index x))
             (#x29 (list :set-font x))
             (#x33 (list :bcd x))
             (#x55 (list :store-regs x))
             (#x65 (list :load-regs x)))))))
```

### 基本命令の実装

```lisp
;; クリアスクリーン命令
(defmethod execute-instruction ((cpu cpu) (display display-system) (instruction (eql :clear-screen)))
  "画面をクリアする"
  (clear-display display)
  (incf (cpu-program-counter cpu) 2))

;; ジャンプ命令
(defmethod execute-instruction ((cpu cpu) display (instruction cons))
  "ジャンプ命令を実行"
  (when (eq (first instruction) :jump)
    (setf (cpu-program-counter cpu) (second instruction))))

;; レジスタ設定命令
(defmethod execute-instruction ((cpu cpu) display (instruction cons))
  "レジスタに値を設定"
  (when (eq (first instruction) :set-reg)
    (let ((reg (second instruction))
          (value (third instruction)))
      (setf (aref (cpu-registers cpu) reg) value)
      (incf (cpu-program-counter cpu) 2))))
```

## テストフレームワークのセットアップ

### 基本テストの作成

```lisp
(in-package #:cl-chip8-tests)

(def-suite chip8-tests
  :description "CHIP-8エミュレーターのテストスイート")

(in-suite chip8-tests)

(test cpu-initialization
  "CPUが正しく初期化されることを確認"
  (let ((cpu (make-instance 'cpu)))
    (is (= (cpu-program-counter cpu) #x200))
    (is (= (cpu-stack-pointer cpu) 0))
    (is (= (cpu-index-register cpu) 0))
    (is (every #'zerop (cpu-registers cpu)))))

(test instruction-decoding
  "命令デコードが正しく動作することを確認"
  (is (eq (decode-instruction #x00E0) :clear-screen))
  (is (equal (decode-instruction #x1234) '(:jump #x234)))
  (is (equal (decode-instruction #x6512) '(:set-reg 5 #x12))))

(test basic-instructions
  "基本命令の実行テスト"
  (let ((cpu (make-instance 'cpu))
        (display (make-instance 'display-system)))

    ;; レジスタ設定テスト
    (execute-instruction cpu display '(:set-reg 1 #x42))
    (is (= (aref (cpu-registers cpu) 1) #x42))

    ;; ジャンプテスト
    (execute-instruction cpu display '(:jump #x300))
    (is (= (cpu-program-counter cpu) #x300))))

(defun run-all-tests ()
  "全テストを実行"
  (run! 'chip8-tests))
```

## 実行とテスト

### SBCLでの実行

```lisp
;; Lispファイルの読み込み
(load "cl-chip8.asd")
(asdf:load-system :cl-chip8)

;; テストの実行
(asdf:test-system :cl-chip8)

;; 基本的な使用例
(in-package :cl-chip8)

(let ((cpu (make-instance 'cpu)))
  ;; CPU状態の確認
  (format t "PC: ~4,'0X~%" (cpu-program-counter cpu))
  (format t "Registers: ~A~%" (cpu-registers cpu)))
```

## 次のステップ

このチュートリアルで基本的な骨組みができました。次のチュートリアルでは：

1. **メモリ管理システム**の実装
2. **完全な命令セット**の実装
3. **表示システム**の構築
4. **入力処理**の実装
5. **音声システム**の追加

各ステップで段階的に機能を追加し、最終的に完全なCHIP-8エミュレーターを完成させます。

`★ Insight ─────────────────────────────────────`
このチュートリアルではCommon Lispの強力な型システムと最適化指示を活用しています。型宣言により実行時のパフォーマンスが大幅に向上し、CLOSによる柔軟な設計が可能になります。マクロシステムの活用により、命令実装が非常に読みやすくなることも重要なポイントです。
`─────────────────────────────────────────────────`