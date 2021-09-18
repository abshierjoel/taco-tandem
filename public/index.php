

<!DOCTYPE html>
<html lang="en">
  <head>

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

        readfile("main.html");

    ?>
    