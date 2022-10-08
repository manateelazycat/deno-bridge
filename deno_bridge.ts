import { WebSocketClient, WebSocketServer, StandardWebSocketClient } from "https://deno.land/x/websocket@v0.1.4/mod.ts";

class DenoBridge {
    appName:string;
    denoPort:string;
    emacsPort:string;
    
    server:WebSocketServer;
    client:WebSocketClient;
    
    messageHandler:(message:string) => void;

    constructor(appName:string, denoPort:string, emacsPort:string, messageHandler:(message:string) => void) {
        this.appName = appName
        this.denoPort = denoPort
        this.emacsPort = emacsPort
        this.messageHandler = messageHandler

        this.server = new WebSocketServer(parseInt(this.denoPort));
        this.server.on("connection", (client: WebSocketClient) => {
            client.on("message", (message: string) => {
                this.messageHandler(message)
            });
        });

        this.client = new StandardWebSocketClient("ws://127.0.0.1:" + emacsPort);
        this.client.on("open", function() {
            console.log("Deno bridge connected!");
        });
    }

    messageToEmacs(message: string) {
        this.client.send(JSON.stringify({
            "type": "show-message",
            "content": message
        }))
    }

    evalInEmacs(code: string) {
        this.client.send(JSON.stringify({
            "type": "eval-code",
            "content": code
        }))
    }

    getEmacsVar(varName: string) {
        return new Promise((resolve, _) => {
            const client: WebSocketClient = new StandardWebSocketClient("ws://127.0.0.1:" + this.emacsPort);
            client.on("message", function (message) {
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

}

new DenoBridge(Deno.args[0], Deno.args[1], Deno.args[2], (message: string) => { console.log("********* ", message) })
