import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

// Configures the React development server for local-network access on mobile devices.
export default defineConfig({
  plugins: [react()],
})
