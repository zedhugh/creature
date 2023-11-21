let ws = new WebSocket("ws://127.0.0.1:4000");
ws.onopen = () => {
    console.log('websocket opened');
}

ws.onclose = () => {
    console.log('websocket closed');
}

ws.onmessage = (e) => {
    console.log('recv:', e.data)
}

let timeout = 0;
let timer = null;

/**
 * 发送消息到 Emacs
 * @param {string} msg 消息内容
 */
function sendMsgToEmacs(msg) {
    if (!(ws instanceof WebSocket) || ws.readyState !== WebSocket.OPEN) return;

    ws.send(msg);
}

const range = 10;

function calcNextTimeout() {
    return (Math.round(Math.random() * range) + 2) * 1000;
}

function schedule() {
    sendMsgToEmacs(window.crypto.randomUUID());
    window.clearTimeout(timer);
    timeout = calcNextTimeout();

    timer = window.setTimeout(schedule, timeout);
}

schedule();
