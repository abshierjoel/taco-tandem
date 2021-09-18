import "./main.css";
import "../node_modules/animate.css";
import { Elm } from "./Main.elm";
import "animate.css";
import * as serviceWorker from "./serviceWorker";
import { decode } from "html-entities";

const app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: {
    gqlUrl: process.env.ELM_APP_GRAPHQL_URL,
    title: "Taco Tandem",
    description: "A Blog to Share the Love of Tacos!",
  },
});

app.ports.sendNewOpenGraph.subscribe((data) => {
  if (data) {
    document
      .querySelector('meta[property="og:title"]')
      .setAttribute("content", `Taco Tandem - ${data.title}`);

    document
      .querySelector('meta[property="og:type"]')
      .setAttribute("content", "article");

    document
      .querySelector('meta[property="og:image"]')
      .setAttribute("content", data.featuredImage.sourceUrl);

    const strippedExcerpt = data.excerpt.replace(/(<([^>]+)>)/gi, "");
    const decodeHtmlEntities = decode(strippedExcerpt);

    document
      .querySelector('meta[property="og:description"]')
      .setAttribute("content", decodeHtmlEntities);

    document
      .querySelector('meta[property="og:url"]')
      .setAttribute("content", `http://www.tacotandem.com${data.uri}`);
  }
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
