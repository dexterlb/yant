export {
    AttachedFile,
    getFile, saveFile,
    getCard, saveCard,
    exportData, nukeAllData, importData
}

import * as localforage from 'localforage'
import * as FileSaver   from 'file-saver'

interface AttachedFile {
    name: string,
    hash: string,
    mime_type: string,
}

interface FileData {
    hash: string,
    data_url: string,
}

interface CardContent {
    text: string,
    done: boolean,
    attached_files?: Array<AttachedFile>,
}

interface Card {
    id: string,
    content: CardContent,
    children: Array<string>,
}

interface CardObject {
    t: 'card'
    card: Card
}

interface FileObject {
    t: 'file'
    file: FileData
}

type Object = CardObject | FileObject

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

async function iterateChildren(f: ((o: Object) => void), card_id: string) {
    let card = await getCard(card_id)
    if (card == null) {
        return
    }

    f({t: 'card', card: card})

    if (card.content.attached_files) {
        for (let af of card.content.attached_files) {
            let data_url = await getFile(af.hash)
            if (data_url) {
                f({t: 'file', file: {hash: af.hash, data_url: data_url}})
            }
        }
    }

    for (let child_id of card.children) {
        await iterateChildren(f, child_id)
    }
}

async function iterateAll(f: ((o: Object) => void)) {
    await iterateChildren(f, "root")
}

interface AllData {
    cards: Array<Card>,
    files: Array<FileData>,
}

async function nukeAllData() {
    await localforage.clear()
}

async function getAllData(): Promise<AllData> {
    let result: AllData = { cards: [], files: [] }

    await iterateAll(o => {
        switch (o.t) {
            case 'card':
                result.cards.push(o.card)
                break;
            case 'file':
                result.files.push(o.file)
                break;
        }
    })

    return result
}

async function saveAllData(data: AllData) {
    await nukeAllData()
    for (let card of data.cards) {
        await saveCard(card)
    }
    for (let file of data.files) {
        await saveFile(file.hash, file.data_url)
    }
}

async function browseForFiles(): Promise< FileList > {
    let file_input = document.createElement('input') as HTMLInputElement;
    file_input.type = 'file';

    let result = new Promise<FileList>((ok, err) => {
        file_input.onchange = e => {
            let target = e.target as HTMLInputElement;
            if (!target.files) {
                return
            }

            ok(target.files)
        }
    })

    file_input.click()

    return result
}

async function exportData() {
    let data = await getAllData()
    let f = new Blob([JSON.stringify(data)], {type: "text/plain;charset=utf-8"})
    FileSaver.saveAs(f, "data.json")
}

async function importData() {
    let files = await browseForFiles()
    if (!files) {
        console.log('no files selected for import')
        return
    }

    let data = JSON.parse(await files[0].text()) as AllData
    await saveAllData(data)
}
