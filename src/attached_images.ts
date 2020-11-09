import * as localforage from 'localforage';

import * as storage from './storage'

export class AttachedImage extends HTMLElement {
    constructor() {
        super();
    }

    connectedCallback() {
        let hash  = this.getAttribute('hash')
        let alt   = this.getAttribute('alt')
        let title = this.getAttribute('title')
        console.log("image; hash: ", hash, " title: ", title, " alt: ", alt)
    }
}

customElements.define('attached-image', AttachedImage);
