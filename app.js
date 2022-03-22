import {par} from './calculate_distance.js';

const app = Elm.NumberRun.init({node: document.querySelector('main')});

app.ports.calculateDistance.subscribe(async message => {
    console.log('distance',message);
    const [current,target] = message;
    const d = await par(current,target);
    app.ports.receiveDistance.send(d);
});
