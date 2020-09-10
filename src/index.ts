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

async function process_get_card(ctx: Context, cardID: string): Promise<void> {
    let storage = window.localStorage;
    let card = storage.getItem(cardID)

    if (card == null) {
        ctx.app.ports.gotCard.send({
            id: cardID,
            text: "<empty>",
            children: [],
        })
    } else {
        ctx.app.ports.gotCard.send(card)
    }
}

function main() {
    let app = elm.Elm.Main.init({ node: document.documentElement });

    let ctx = init(app)

    app.ports.getCard.subscribe((cardID: any) => {
        process_get_card(ctx, cardID as string).then(() => {
        }).catch(err => {
            console.log('error while processing get_card request for id', cardID, ': ', err)
        })
    });
}

main()
