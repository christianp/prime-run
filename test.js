import {PrimeGenerator} from './calculate_distance.js';

const g = new PrimeGenerator();

function par(current,target) {
    const path = [];
    while(current != target) {
        const options = [];
        for(let f of g.prime_factors(current)) {
            if(current-f>=2) {
                options.push(current-f);
            }
            options.push(current+f);
        }
        let best = Math.abs(current-target);
        let bo;
        for(let o of options) {
            const d = Math.abs(o-target);
            if(d<best) {
                best = d;
                bo = o;
            }
        }
        current = bo;
        path.push(current);
    }
    document.getElementById('output').innerHTML = `
<p>${path.length} steps</p>
<p>${path.join(' → ')}</p>
`;
}

function go() {
    const current = parseInt(document.getElementById('current').value);
    const target = parseInt(document.getElementById('target').value);
    par(current,target);
}
document.getElementById('go').addEventListener('click',go);
