import renderMathInElement from 'katex/dist/contrib/auto-render'

require('katex/dist/katex.css')

const mathsOptions = {
    delimiters: [
        {left: "$$", right: "$$", display: true},
        {left: "$", right: "$", display: false},
        {left: "\(", right: "\)", display: false},
        {left: "\[", right: "\]", display: true},
    ]
};

export class MathsContainer extends HTMLElement {
    constructor() {
        super();
    }

    connectedCallback() {
        renderMathInElement(this, mathsOptions);
    }
}

customElements.define('may-contain-maths', MathsContainer);
