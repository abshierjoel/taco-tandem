import './main.css';
import '../node_modules/animate.css';
import { Elm } from './Main.elm';
import 'animate.css';
import * as serviceWorker from './serviceWorker';

const app = Elm.Main.init({
  node: document.getElementById('root'),
});

app.ports.sendNewOpenGraph.subscribe((data) => {
  if (data) {
    document
      .querySelector('meta[property="og:title"]')
      .setAttribute('content', `Taco Tandem - ${data.title}`);

    const strippedExcerpt = data.excerpt.replace(/(<([^>]+)>)/gi, '');

    document
      .querySelector('meta[property="og:description"]')
      .setAttribute('content', strippedExcerpt);

    document
      .querySelector('meta[property="og:url"]')
      .setAttribute('content', `http://www.tacotandem.com${data.uri}`);
  }
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
