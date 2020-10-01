let typesetter = Promise.resolve();  // Used to hold chain of typesetting calls
declare global {
    var MathJax: any
}

require('mathjax/es5/tex-svg.js');

function typeset(code: () => Array<HTMLElement>) {
    typesetter = typesetter.then(() => {return MathJax.typesetPromise(code());})
        .catch((err) => console.log('Typeset failed: ' + err.message));
    return typesetter;
}

export class MathsContainer extends HTMLElement {
    constructor() {
        super();
    }

    connectedCallback() {
        typeset(() => [this]);
    }
}

customElements.define('may-contain-maths', MathsContainer);
