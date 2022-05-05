import { decode } from 'html-entities';
import { registerSW } from 'virtual:pwa-register';

import { Elm } from './Main.elm';

import './main.css';
import '../node_modules/animate.css';
import 'animate.css';

registerSW({ onOfflineReady() {} });

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    gqlUrl: import.meta.env.VITE_ELM_APP_GRAPHQL_URL,
    title: 'Taco Tandem',
    description: 'A Blog to Share the Love of Tacos!',
  },
});

app.ports.sendNewOpenGraph.subscribe((data) => {
  if (data) {
    document
      .querySelector('meta[property="og:title"]')
      .setAttribute('content', `Taco Tandem - ${data.title}`);

    document
      .querySelector('meta[property="og:type"]')
      .setAttribute('content', 'article');

    document
      .querySelector('meta[property="og:image"]')
      .setAttribute('content', data.featuredImage.sourceUrl);

    const strippedExcerpt = data.excerpt.replace(/(<([^>]+)>)/gi, '');
    const decodeHtmlEntities = decode(strippedExcerpt);

    document
      .querySelector('meta[property="og:description"]')
      .setAttribute('content', decodeHtmlEntities);

    document
      .querySelector('meta[property="og:url"]')
      .setAttribute('content', `http://www.tacotandem.com${data.uri}`);
  }
});
