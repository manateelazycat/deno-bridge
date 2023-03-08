English | [简体中文](./README.zh-CN.md)

<p align="center">
  <br>A free/libre framework that build bridge between Emacs and Deno runtime. <br>Allows execution of JavaScript and Typescript within Emacs.
</p>

## Vision
With deno-bridge, we can execution of JavaScript and Typescript within Emacs and don't need change source code of Emacs. It's bringing TypeScript ecosystem of powerful tools and approaches that Emacs just doesn't have currently:

1. TypeScript offers an extremely flexible typing system, that allows to user to have compile time control of their scripting, with the flexibility of types "getting out of the way" when not needed.
2. Deno uses Google's v8 JavaScript engine, which features an extremely powerful JIT and world-class garbage collector.
3. Usage of modern Async I/O utilizing Rust's Tokio library.
4. WebWorker support, meaning that multiple JavaScript engines can be running in parallel within the editor. The only restriction is that only the 'main' JS Engine can directly call lisp functions.
5. WebAssembly support, compile your C module as WebAsm and distribute it to the world. Don't worry about packaging shared libraries or changing module interfaces, everything can be handled and customized by you the user, at the scripting layer. No need to be dependent on native implementation details.
6. Performance, v8's world-class JIT offers the potential for large performance gains. Async I/O from Deno, WebWorkers, and WebAsm, gives you the tools to make Emacs a smoother and faster experience without having to install additional tools to launch as background processes or worry about shared library versions.

## Install

#### 1. Download deno-bridge

```Bash
git clone --depth=1 -b master https://github.com/manateelazycat/deno-bridge ~/.emacs.d/site-lisp/deno-bridge/
```

#### 2. Install Dependences

1. [Deno](https://github.com/denoland/deno_install)
2. [Websocket](https://github.com/ahyatt/emacs-websocket)

#### 3. Add to ~/.emacs

From here on, you can add the full path to the deno-bridge installation directory to your Emacs ```load-path```, then add the following to `init.el`:

```Elisp
(add-to-list 'load-path "~/.emacs.d/site-lisp/deno-bridge/")
(require 'deno-bridge)
```

## Example

I write a demo to show you how simple write app base on deno-brige:

#### Elisp (deno-bridge-demo.el)

```elisp
(require 'deno-bridge)
(setq deno-bridge-demo-ts-path (concat (file-name-directory load-file-name) "deno-bridge-demo.ts"))
(deno-bridge-start "demo" deno-bridge-demo-ts-path)
(deno-bridge-call "demo" "ping" "Hello from Emacs.")
```

1. Start Deno process: `(deno-bridge-start "demo" deno-bridge-demo-ts-path)`
2. Call TypeScript function from Emacs: `(deno-bridge-call "demo" "ping" "Hello from Emacs.")`
3. Clean Deno process: execute command `deno-bridge-exit` and select application name

#### TypeScript (deno-bridge-demo.ts)

```typescript
import { DenoBridge } from "https://deno.land/x/denobridge@0.0.1/mod.ts"

const bridge = new DenoBridge(Deno.args[0], Deno.args[1], Deno.args[2], messageDispatcher)

async function messageDispatcher(message: string) {
    const [funcName, funcArgs] = JSON.parse(message)[1]
    
    if (funcName == "ping") {
        console.log("Emacs message: ", funcArgs)

        const emacsVar = await bridge.getEmacsVar("deno-bridge-app-list")
        console.log("Emacs var 'deno-bridge-app-list': ", emacsVar)

        bridge.messageToEmacs("Hi from TypeScript")

        bridge.evalInEmacs('(message \"Eval from TypeScript\")')
    }
}
```

1. Create DenoBridge object to communicate with Emacs
2. Get Emacs variable value: `await bridge.getEmacsVar(emacs-var-name)`
3. Show message in Emacs minibuffer: `bridge.messageToEmacs("message")`
4. Eval Elisp code in TypeScript: `bridge.evalInEmacs('(message \"Eval from TypeScript\")')`

**That's all story about deno-bridge.**

## Project base on deno-bridge

* [deno-bridge-jieba](https://github.com/ginqi7/deno-bridge-jieba)
* [emmet2-mode](https://github.com/P233/emmet2-mode)
* [insert-translated-name](https://github.com/manateelazycat/insert-translated-name)
* [deno-bridge-echo](https://github.com/nailuoGG/deno-bridge-echo)

## Contributor
<a href = "https://github.com/manateelazycat/deno-bridge/graphs/contributors">
  <img src = "https://contrib.rocks/image?repo=manateelazycat/deno-bridge"/>
</a>
