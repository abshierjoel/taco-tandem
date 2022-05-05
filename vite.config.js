import { defineConfig } from 'vite';
import { plugin as elm } from 'vite-plugin-elm';
import { VitePWA } from 'vite-plugin-pwa';

import pwaConfig from './pwa.config.js';

// https://vitejs.dev/config/
export default defineConfig({
  base: '/',
  plugins: [elm(), VitePWA(pwaConfig)],
});
