let fragment = new URLSearchParams(window.location.hash.substr(1));
let theme = fragment.get('theme');

// do nothing with theme yet. todo.
require('./styles/main.scss');

let elm = require('./Main.elm')

require('./maths.ts')
require('./fonts.ts')

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
    let card = storage.getItem('card_' + cardID)

    if (card == null) {
        console.log("returning empty card at ", cardID)
        ctx.app.ports.gotCard.send({
            id: cardID,
            content: {
                text: "",
                done: false,
            },
            children: [],
        })
    } else {
        console.log("returning card at ", cardID, ": ", JSON.parse(card))
        ctx.app.ports.gotCard.send(JSON.parse(card))
    }
}

async function process_save_card(ctx: Context, card: any): Promise<void> {
    let id = card.id;
    if (!id) {
        throw new Error("invalid card id - " + id)
    }

    console.log("saving card at ", card.id, " : ", card)

    let storage = window.localStorage;
    storage.setItem('card_' + id, JSON.stringify(card))
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
    app.ports.saveCard.subscribe((card: any) => {
        process_save_card(ctx, card).then(() => {
        }).catch(err => {
            console.log('error while processing save_card request for ', card, ': ', err)
        })
    });
}

main()
