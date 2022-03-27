import {par} from './calculate_distance.js';

const storageKey = 'prime-run';

const save = JSON.parse(localStorage.getItem(storageKey) || '{"current_level": 0, "max_level": 0}');
console.log(save);

const app = Elm.PrimeRun.init({node: document.querySelector('main'), flags: save});

app.ports.calculateDistance.subscribe(async message => {
    console.log('distance',message);
    const [current,target] = message;
    const d = await par(current,target);
    app.ports.receiveDistance.send(d);
});

app.ports.saveState.subscribe(async message => {
    console.log('save',message);
    localStorage.setItem(storageKey,JSON.stringify(message));
});
