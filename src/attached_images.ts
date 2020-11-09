import * as localforage from 'localforage';

import * as storage from './storage.ts'

interface ImageCache {
    [key: string]: HTMLImageElement,
}
var imageCache: ImageCache = {}

export class AttachedImage extends HTMLElement {
    constructor() {
        super();
    }

    connectedCallback() {
        let hash  = this.getAttribute('hash')
        let alt   = this.getAttribute('alt')
        let title = this.getAttribute('title')
        let cacheKey = JSON.stringify({hash: hash, alt: alt, title: title})
        let cachedImage = imageCache[cacheKey]
        if (cachedImage != undefined) {
            console.log('got image from cache ', cacheKey, cachedImage)
            this.innerHTML = ''
            this.appendChild(cachedImage.cloneNode(true))
            return
        }
        console.log('image not in cache ', cacheKey)

        let fail = () => {
            this.innerHTML = '<div class="image-failed">failed loading image.</div>'
        }

        if (hash == null) {
            fail()
            return
        }

        this.innerHTML = '<div class="image-loading">loading image...</div>'

        storage.getFile(hash)
            .catch(err => {
                console.log('failed getting image! ', err)
                fail()
            })
            .then(data_url => {
                if (data_url == null) {
                    console.log('image is missing! hash: ', hash)
                    return
                }

                let img = new Image()

                if (alt != null) {
                    img.alt = alt
                }
                if (title != null) {
                    img.title = title
                }

                img.src = data_url

                imageCache[cacheKey] = img

                this.innerHTML = ''
                this.appendChild(img)
            })
    }
}

customElements.define('attached-image', AttachedImage);
