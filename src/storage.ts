export {
    AttachedFile,
    getFile, saveFile,
    getCard, saveCard,
}

import * as localforage from 'localforage'

interface AttachedFile {
    name: string,
    hash: string,
    mime_type: string,
}

interface CardContent {
    text: string,
    done: boolean,
}

interface Card {
    id: string,
    content: CardContent,
    children: Array<string>,
}

async function saveCard(card: Card) {
    await localforage.setItem('card_' + card.id, JSON.stringify(card))
}

async function getCard(id: string): Promise<Card | null> {
    let data = await localforage.getItem('card_' + id) as (string | null)
    if (data == null) {
        return null
    }

    return JSON.parse(data) as Card
}

async function saveFile(hash: string, data_url: string) {
    await localforage.setItem('file_' + hash, data_url)
}

async function getFile(hash: string): Promise<string | null> {
    return await localforage.getItem('file_' + hash) as (string | null)
}
