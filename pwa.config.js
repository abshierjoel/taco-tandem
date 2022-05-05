const pwaConfig = {
  includeAssets: [
    'favicon.ico',
    'robots.txt',
    // 'apple-touch-icon.png',
    'taco.svg',
  ],
  manifest: {
    short_name: 'Taco Tandem',
    name: 'Taco Tandem - A Blog to Share the Love of Tacos!',
    description:
      'A one stop shop for everything taco related -- restaurant reviews, recipes, general taco info -- if it’s taco related, it’ll probably make its way here! Whether you are a fellow taco devotee or you are just beginning to discover non-Taco Bell tacos, join us on our adventure exploring the world of tacos.',
    theme_color: '#e0485d',
    background_color: '#322885',
    icons: [
      {
        src: '/taco.svg',
        sizes: '500x500',
        type: 'image/svg+xml',
        purpose: 'any',
      },
    ],
    start_url: '/',
    display: 'standalone',
    scope: '/',
  },
};

export default pwaConfig;
