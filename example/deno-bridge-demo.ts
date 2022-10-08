import { DenoBridge } from "https://deno.land/x/denobridge@0.0.1/mod.ts"

const bridge = new DenoBridge(Deno.args[0], Deno.args[1], Deno.args[2], messageDispatcher)

async function messageDispatcher(message: string) {
    const info = JSON.parse(message)
    if (info[0] == "ping") {
        console.log("Emacs message: ", info[1][0])

        const emacsVar = await bridge.getEmacsVar("deno-bridge-app-list")
        console.log("Emacs var 'deno-bridge-app-list': ", emacsVar)

        bridge.messageToEmacs("Hi from TypeScript")

        bridge.evalInEmacs('(message \"Eval from TypeScript\")')
    }
}
