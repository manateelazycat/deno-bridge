import { WebSocketClient, WebSocketServer, StandardWebSocketClient } from "https://deno.land/x/websocket@v0.1.4/mod.ts";

const args = Deno.args;
const denoPort = args[1];
const emacsPort = args[2];

// Show message in Emacs minibuffer.
function messageToEmacs(message) {
    client.send(JSON.stringify({
        "type": "show-message",
        "content": message
    }))
}

function evalInEmacs(code) {
    client.send(JSON.stringify({
        "type": "eval-code",
        "content": code
    }))
}

// Get Emacs variable with `await getEmacsVar`.
function getEmacsVar(varName) {
    return new Promise((resolve, _) => {
        const client: WebSocketClient = new StandardWebSocketClient("ws://127.0.0.1:" + emacsPort);
        client.on("message", function (message: string) {
            resolve(message["data"]);
        });

        client.on("open", function() {
            client.send(JSON.stringify({
                "type": "fetch-var",
                "content": varName
            }));

        });
    })
}

const server = new WebSocketServer(denoPort.toString());
server.on("connection", function (client: WebSocketClient) {
    client.on("message", function (message: string) {
    });
});

const client: WebSocketClient = new StandardWebSocketClient("ws://127.0.0.1:" + emacsPort);
client.on("open", function() {
    console.log("Deno bridge connected!");
});
