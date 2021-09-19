

<!DOCTYPE html>
<html lang="en">
  <head>

    <link rel="manifest" href="/manifest.json">

    <?php 

        $url = $_SERVER['REQUEST_URI'];
        $explode = explode('/', $url);

        if(isset($explode[1]) && isset($explode[2]) && ($explode[1] == 'post'))
        {
            $ch = curl_init();
            curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
            curl_setopt($ch, CURLOPT_URL, 
                "tacotandem.com/wordpress/wp-json/wp/v2/posts?slug=" . $explode[2] . "&_embed"
            );
            
            $content = curl_exec($ch);

            if(curl_getinfo($ch, CURLINFO_HTTP_CODE) != 200) {
                getDefaultOpenGraph();
                return;
            }

            $post = json_decode($content)[0];

            $featuredImageUrl = $post->_embedded->{'wp:featuredmedia'}[0]->source_url;

            ?>

                <meta property="og:url" content="http://tacotandem.com/post/<?php echo $post->slug; ?>" />
                <meta property="og:type" content="article" />
                <meta
                    property="og:title"
                    content="Taco Tandem - <?php echo $post->title->rendered; ?>"
                />
                <meta
                    property="og:description"
                    content="<?php echo strip_tags($post->excerpt->rendered); ?>"
                />
                <meta
                    property="og:image"
                    content="<?php echo $featuredImageUrl ?>"
                />

            <?php
        }
        else
        {
            getDefaultOpenGraph();
        }

        function getDefaultOpenGraph() 
        {
            ?>

                <meta property="og:url" content="http://tacotandem.com/" />
                <meta property="og:type" content="article" />
                <meta
                    property="og:title"
                    content="Taco Tandem - A Blog to Share the Love of Tacos!"
                />
                <meta
                    property="og:description"
                    content="A one stop shop for everything taco related -- restaurant reviews, recipes, general taco info -- if it’s taco related, it’ll probably make its way here! Whether you are a fellow taco devotee or you are just beginning to discover non-Taco Bell tacos, join us on our adventure exploring the world of tacos."
                />
                <meta
                    property="og:image"
                    content="http://tacotandem.com/img/taco-tandem-logo.png"
                />

            <?php
        }

    ?>

    <meta charset="utf-8" />
    <meta http-equiv="x-ua-compatible" content="ie=edge" />
    <meta
      name="viewport"
      content="width=device-width,initial-scale=1,shrink-to-fit=no"
    />
    <meta name="theme-color" content="#000000" />
    <link rel="manifest" href="/manifest.json" />
    <link rel="shortcut icon" type="image/png" href="/favicon.png" />
    <title>Taco Tandem - A blog to share the love of tacos!</title>
    <script
      async
      src="https://www.googletagmanager.com/gtag/js?id=G-3DCD3DDXHV"
    ></script>
    <script>
      function gtag() {
        dataLayer.push(arguments);
      }
      (window.dataLayer = window.dataLayer || []),
        gtag('js', new Date()),
        gtag('config', 'G-3DCD3DDXHV');
    </script>
    <link href="/static/css/vendors~main.276ef6a7.chunk.css" rel="stylesheet" />
    <link href="/static/css/main.8fe59ba6.chunk.css" rel="stylesheet" />
    
  </head>
  <body>
    <noscript>You need to enable JavaScript to visit Taco Tandem.</noscript>
    <!-- <div id="root"></div> -->
    <script src="/static/js/runtime~main.1b922744.js"></script>
    <script src="/static/js/vendors~main.ffb49ccd.chunk.js"></script>
    <script src="/static/js/main.5ecac340.chunk.js"></script>
  </body>
</html>
