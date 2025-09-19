# API リファレンス

## パッケージ: CL-CHIP8

### 概要

`cl-chip8` パッケージは、世界最高峰のCHIP-8エミュレーターを構築するためのAPIを提供します。Common Lispの強力な機能を活用し、高性能で拡張可能な設計を実現しています。

## エクスポートされるシンボル

### クラス

#### `CHIP8-EMULATOR`
**基底クラス:** `STANDARD-OBJECT`

メインエミュレーターファサードクラス。すべてのサブシステムを統合管理します。

**スロット:**
- `cpu` - CPUインスタンス（型: `CPU`）
- `memory` - メモリマネージャー（型: `MEMORY-MANAGER`）
- `display` - 表示システム（型: `DISPLAY-SYSTEM`）
- `input` - 入力ハンドラー（型: `INPUT-HANDLER`）
- `audio` - 音声システム（型: `AUDIO-SYSTEM`）
- `running` - 実行状態フラグ（型: `BOOLEAN`）
- `clock-speed` - クロック速度（型: `POSITIVE-INTEGER`、デフォルト: 500）

**例:**
```lisp
(defparameter *emulator* (make-instance 'chip8-emulator))
```

---

#### `CPU`
**基底クラス:** `STANDARD-OBJECT`

CHIP-8 CPU の状態と動作を管理します。

**スロット:**
- `registers` - 汎用レジスタV0-VF（型: `CHIP8-REGISTERS`）
- `index-register` - Iレジスタ（型: `WORD-VALUE`）
- `program-counter` - プログラムカウンタ（型: `ADDRESS`）
- `stack-pointer` - スタックポインタ（型: `BYTE-VALUE`）
- `stack` - サブルーチンスタック（型: `CHIP8-STACK`）
- `delay-timer` - 遅延タイマー（型: `BYTE-VALUE`）
- `sound-timer` - 音声タイマー（型: `BYTE-VALUE`）

**例:**
```lisp
(defparameter *cpu* (make-instance 'cpu))
(setf (aref (cpu-registers *cpu*) 0) #x42)
```

---

#### `MEMORY-MANAGER`
**基底クラス:** `STANDARD-OBJECT`

メモリアクセスと保護機能を提供します。

**スロット:**
- `memory-data` - 4KBメインメモリ（型: `CHIP8-MEMORY`）
- `font-data` - フォントデータ位置（型: `ADDRESS`）
- `access-log` - アクセスログ（型: `LIST`、デバッグ用）

**例:**
```lisp
(defparameter *memory* (make-instance 'memory-manager))
(load-font-data *memory*)
```

---

#### `DISPLAY-SYSTEM`
**基底クラス:** `STANDARD-OBJECT`

64x32ピクセルの表示システムを管理します。

**スロット:**
- `framebuffer` - フレームバッファ（型: `CHIP8-DISPLAY`）
- `scale-factor` - 表示スケール（型: `POSITIVE-INTEGER`、デフォルト: 10）
- `foreground-color` - 前景色（型: `RGB-COLOR`）
- `background-color` - 背景色（型: `RGB-COLOR`）

**例:**
```lisp
(defparameter *display* (make-instance 'display-system))
(clear-display *display*)
```

## 関数

### エミュレーター制御

#### `MAKE-EMULATOR (&key clock-speed)`
**戻り値:** `CHIP8-EMULATOR`

新しいエミュレーターインスタンスを作成します。

**パラメータ:**
- `clock-speed` - クロック速度（Hz）、デフォルト: 500

**例:**
```lisp
(defparameter *emu* (make-emulator :clock-speed 1000))
```

---

#### `LOAD-ROM (emulator rom-path)`
**戻り値:** `ADDRESS`

ROMファイルをエミュレーターのメモリにロードします。

**パラメータ:**
- `emulator` - エミュレーターインスタンス（型: `CHIP8-EMULATOR`）
- `rom-path` - ROMファイルパス（型: `PATHNAME`）

**戻り値:** ロードアドレス

**例外:**
- `FILE-ERROR` - ファイルが見つからない場合
- `MEMORY-ERROR` - ROMサイズが大きすぎる場合

**例:**
```lisp
(load-rom *emulator* #P"roms/pong.ch8")
```

---

#### `RUN-EMULATOR (emulator &key max-cycles callback)`
**戻り値:** `INTEGER`

エミュレーターを実行します。

**パラメータ:**
- `emulator` - エミュレーターインスタンス（型: `CHIP8-EMULATOR`）
- `max-cycles` - 最大実行サイクル数（型: `POSITIVE-INTEGER`、省略可）
- `callback` - 各サイクル後に呼び出される関数（型: `FUNCTION`、省略可）

**戻り値:** 実行されたサイクル数

**例:**
```lisp
;; 無限実行
(run-emulator *emulator*)

;; 1000サイクル実行
(run-emulator *emulator* :max-cycles 1000)

;; コールバック付き実行
(run-emulator *emulator*
              :callback (lambda (cycle-count)
                         (when (zerop (mod cycle-count 100))
                           (format t "Cycle: ~A~%" cycle-count))))
```

---

#### `STEP-EMULATOR (emulator)`
**戻り値:** `BOOLEAN`

エミュレーターを1サイクル進めます。

**パラメータ:**
- `emulator` - エミュレーターインスタンス（型: `CHIP8-EMULATOR`）

**戻り値:** 実行継続可能な場合 `T`、停止した場合 `NIL`

**例:**
```lisp
(loop while (step-emulator *emulator*)
      do (handle-events))
```

---

#### `RESET-EMULATOR (emulator)`
**戻り値:** `CHIP8-EMULATOR`

エミュレーターをリセットし、初期状態に戻します。

**パラメータ:**
- `emulator` - エミュレーターインスタンス（型: `CHIP8-EMULATOR`）

**例:**
```lisp
(reset-emulator *emulator*)
```

### CPU操作

#### `CPU-REGISTERS (cpu)`
**戻り値:** `CHIP8-REGISTERS`

CPUの汎用レジスタ配列を取得します。

**パラメータ:**
- `cpu` - CPUインスタンス（型: `CPU`）

**例:**
```lisp
(aref (cpu-registers *cpu*) 0)  ; V0の値を取得
(setf (aref (cpu-registers *cpu*) 1) #xFF)  ; V1に値を設定
```

---

#### `CPU-PROGRAM-COUNTER (cpu)`
#### `(SETF CPU-PROGRAM-COUNTER) (value cpu)`
**戻り値:** `ADDRESS`

プログラムカウンタの値を取得・設定します。

**例:**
```lisp
(cpu-program-counter *cpu*)  ; 現在のPC取得
(setf (cpu-program-counter *cpu*) #x200)  ; PCを#x200に設定
```

---

#### `CPU-STACK-POINTER (cpu)`
#### `(SETF CPU-STACK-POINTER) (value cpu)`
**戻り値:** `BYTE-VALUE`

スタックポインタの値を取得・設定します。

---

#### `CPU-INDEX-REGISTER (cpu)`
#### `(SETF CPU-INDEX-REGISTER) (value cpu)`
**戻り値:** `WORD-VALUE`

インデックスレジスタ（I）の値を取得・設定します。

### メモリ操作

#### `READ-MEMORY (memory-manager address)`
**戻り値:** `BYTE-VALUE`

指定アドレスからメモリを読み取ります。

**パラメータ:**
- `memory-manager` - メモリマネージャーインスタンス（型: `MEMORY-MANAGER`）
- `address` - 読み取りアドレス（型: `ADDRESS`）

**例外:**
- `MEMORY-BOUNDS-ERROR` - 不正なアドレス
- `MEMORY-PROTECTION-ERROR` - 読み取り保護違反

**例:**
```lisp
(read-memory *memory* #x200)
```

---

#### `WRITE-MEMORY (memory-manager address value)`
**戻り値:** `BYTE-VALUE`

指定アドレスにメモリを書き込みます。

**パラメータ:**
- `memory-manager` - メモリマネージャーインスタンス（型: `MEMORY-MANAGER`）
- `address` - 書き込みアドレス（型: `ADDRESS`）
- `value` - 書き込み値（型: `BYTE-VALUE`）

**例外:**
- `MEMORY-BOUNDS-ERROR` - 不正なアドレス
- `MEMORY-PROTECTION-ERROR` - 書き込み保護違反

**例:**
```lisp
(write-memory *memory* #x300 #x42)
```

---

#### `LOAD-FONT-DATA (memory-manager)`
**戻り値:** `ADDRESS`

標準フォントデータをメモリにロードします。

**パラメータ:**
- `memory-manager` - メモリマネージャーインスタンス（型: `MEMORY-MANAGER`）

**戻り値:** フォントデータの開始アドレス

**例:**
```lisp
(load-font-data *memory*)
```

### 表示システム

#### `CLEAR-DISPLAY (display-system)`
**戻り値:** `DISPLAY-SYSTEM`

画面をクリアします（すべてのピクセルを0に設定）。

**パラメータ:**
- `display-system` - 表示システムインスタンス（型: `DISPLAY-SYSTEM`）

**例:**
```lisp
(clear-display *display*)
```

---

#### `DRAW-SPRITE (display-system x y sprite-data)`
**戻り値:** `BOOLEAN`

指定座標にスプライトを描画します。

**パラメータ:**
- `display-system` - 表示システムインスタンス（型: `DISPLAY-SYSTEM`）
- `x` - X座標（型: `BYTE-VALUE`）
- `y` - Y座標（型: `BYTE-VALUE`）
- `sprite-data` - スプライトデータ（型: `LIST`）

**戻り値:** ピクセル衝突が発生した場合 `T`

**例:**
```lisp
(draw-sprite *display* 10 15 '(#xF0 #x90 #x90 #x90 #xF0))
```

---

#### `GET-PIXEL (display-system x y)`
**戻り値:** `BIT`

指定座標のピクセル値を取得します。

**パラメータ:**
- `display-system` - 表示システムインスタンス（型: `DISPLAY-SYSTEM`）
- `x` - X座標（型: `BYTE-VALUE`）
- `y` - Y座標（型: `BYTE-VALUE`）

**例:**
```lisp
(get-pixel *display* 32 16)
```

### 入力処理

#### `KEY-PRESSED-P (input-handler key)`
**戻り値:** `BOOLEAN`

指定キーが押されているかチェックします。

**パラメータ:**
- `input-handler` - 入力ハンドラーインスタンス（型: `INPUT-HANDLER`）
- `key` - キー番号（型: `(INTEGER 0 15)`）

**例:**
```lisp
(key-pressed-p *input* 5)  ; キー5が押されているかチェック
```

---

#### `WAIT-FOR-KEY (input-handler)`
**戻り値:** `(OR INTEGER NULL)`

キーが押されるまで待機します。

**パラメータ:**
- `input-handler` - 入力ハンドラーインスタンス（型: `INPUT-HANDLER`）

**戻り値:** 押されたキー番号、またはタイムアウト時は `NIL`

**例:**
```lisp
(let ((key (wait-for-key *input*)))
  (when key
    (format t "Key ~A pressed~%" key)))
```

### 音声システム

#### `BEEP (audio-system &optional duration)`
**戻り値:** `AUDIO-SYSTEM`

ビープ音を再生します。

**パラメータ:**
- `audio-system` - 音声システムインスタンス（型: `AUDIO-SYSTEM`）
- `duration` - 再生時間（秒）、省略時はシステム設定による

**例:**
```lisp
(beep *audio*)
(beep *audio* 0.5)  ; 0.5秒間のビープ
```

## デバッグ機能

### `DEBUGGER`
**基底クラス:** `STANDARD-OBJECT`

エミュレーターのデバッグ機能を提供します。

#### `SET-BREAKPOINT (debugger address)`
**戻り値:** `DEBUGGER`

指定アドレスにブレークポイントを設定します。

**パラメータ:**
- `debugger` - デバッガーインスタンス（型: `DEBUGGER`）
- `address` - ブレークポイントアドレス（型: `ADDRESS`）

**例:**
```lisp
(set-breakpoint *debugger* #x300)
```

---

#### `STEP-DEBUG (debugger)`
**戻り値:** `BOOLEAN`

デバッグステップ実行を行います。

---

#### `DUMP-STATE (debugger &optional output-stream)`
**戻り値:** `DEBUGGER`

現在のエミュレーター状態をダンプします。

**パラメータ:**
- `debugger` - デバッガーインスタンス（型: `DEBUGGER`）
- `output-stream` - 出力ストリーム（型: `STREAM`、デフォルト: `*STANDARD-OUTPUT*`）

**例:**
```lisp
(dump-state *debugger*)
(dump-state *debugger* *debug-output*)
```

## 型定義

### プリミティブ型

#### `BYTE-VALUE`
```lisp
(unsigned-byte 8)
```
8ビット符号なし整数（0-255）

#### `WORD-VALUE`
```lisp
(unsigned-byte 16)
```
16ビット符号なし整数（0-65535）

#### `ADDRESS`
```lisp
(unsigned-byte 16)
```
メモリアドレス（0x000-0xFFF）

#### `REGISTER-INDEX`
```lisp
(integer 0 15)
```
レジスタインデックス（0-15）

### 配列型

#### `CHIP8-MEMORY`
```lisp
(simple-array (unsigned-byte 8) (4096))
```
4KBメモリ配列

#### `CHIP8-REGISTERS`
```lisp
(simple-array (unsigned-byte 8) (16))
```
16個のレジスタ配列

#### `CHIP8-STACK`
```lisp
(simple-array (unsigned-byte 16) (16))
```
16レベルのスタック配列

#### `CHIP8-DISPLAY`
```lisp
(simple-array bit (64 32))
```
64x32ピクセル表示配列

## 例外

### `CHIP8-ERROR`
**基底クラス:** `ERROR`

すべてのCHIP-8関連エラーの基底クラス

### `MEMORY-ERROR`
**基底クラス:** `CHIP8-ERROR`

メモリ関連エラーの基底クラス

#### `MEMORY-BOUNDS-ERROR`
**基底クラス:** `MEMORY-ERROR`

メモリ境界外アクセスエラー

**スロット:**
- `address` - 問題のあったアドレス

#### `MEMORY-PROTECTION-ERROR`
**基底クラス:** `MEMORY-ERROR`

メモリ保護違反エラー

**スロット:**
- `address` - 問題のあったアドレス
- `access-type` - アクセスタイプ（:READ, :WRITE, :EXECUTE）

### `INSTRUCTION-ERROR`
**基底クラス:** `CHIP8-ERROR`

命令実行関連エラーの基底クラス

#### `INVALID-INSTRUCTION-ERROR`
**基底クラス:** `INSTRUCTION-ERROR`

不正な命令エラー

**スロット:**
- `opcode` - 不正なオペコード

## 設定

### `*DEBUG-MODE*`
**型:** `BOOLEAN`
**デフォルト:** `NIL`

デバッグモードの有効/無効を制御します。

### `*DEFAULT-CLOCK-SPEED*`
**型:** `POSITIVE-INTEGER`
**デフォルト:** `500`

デフォルトのクロック速度（Hz）

### `*FONT-START-ADDRESS*`
**型:** `ADDRESS`
**デフォルト:** `#x050`

フォントデータの開始アドレス

## 使用例

### 基本的な使用例

```lisp
;; エミュレーターの作成と初期化
(defparameter *emulator* (make-emulator :clock-speed 1000))

;; ROMの読み込み
(load-rom *emulator* #P"roms/tetris.ch8")

;; 実行
(run-emulator *emulator*)
```

### デバッグ例

```lisp
;; デバッグモードで実行
(let ((*debug-mode* t))
  (defparameter *debugger* (make-instance 'debugger :emulator *emulator*))

  ;; ブレークポイント設定
  (set-breakpoint *debugger* #x300)

  ;; ステップ実行
  (loop while (step-debug *debugger*)
        do (dump-state *debugger*)))
```

### カスタムコールバック例

```lisp
;; 表示更新コールバック付き実行
(run-emulator *emulator*
              :callback (lambda (cycle)
                         (when (zerop (mod cycle 10))
                           (update-display *emulator*))))
```