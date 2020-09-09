let fragment = new URLSearchParams(window.location.hash.substr(1));
let theme = fragment.get('theme');

// do nothing with theme yet. todo.
require('./styles/main.scss');

let elm = require('./Main.elm')

type ElmApp = any   // he he

interface Context {
    app: ElmApp
}

function init(app: ElmApp): Context {
    return {
        app: app,
    }
}

async function process_message(ctx: Context, msg: any): Promise<void> {
    switch (msg._t) {
        default:
            throw new Error('what do I do with ' + msg._t)
    }
}

function main() {
    let app = elm.Elm.Main.init({ node: document.documentElement });

    let ctx = init(app)

    app.ports.outgoing.subscribe((msg: any) => {
        process_message(ctx, msg).then(() => {
        }).catch(err => {
            console.log('error while processing ', msg, ': ', err)
        })
    });
}

main()
