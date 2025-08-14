# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is the official Node.js SDK for APIFRAME.PRO, a service providing programmatic access to Midjourney AI image generation. The library is a simple wrapper around the APIFRAME REST API endpoints.

## Key Architecture

- **Single-file library**: All functionality is contained in `index.js`
- **Pattern**: Simple API client wrapper using axios for HTTP requests
- **Error handling**: Errors are logged to console and methods return undefined on failure
- **Async operations**: All methods support webhooks for asynchronous result delivery

## Development Commands

This project has no build process or npm scripts defined. Common operations:

```bash
# Install dependencies
npm install

# No test suite exists - manual testing required
# No linting or type checking configured
```

## API Methods Structure

All methods in the `ApiframeClient` class follow a consistent pattern:

1. Accept an options object with required and optional parameters
2. Build a JSON payload and axios config
3. Make POST request to the appropriate endpoint
4. Return the response data or undefined on error
5. Support optional webhook URLs for async notifications

Key methods include:
- `imagine()` - Generate images from text prompts
- `upscale_*()` - Various upscaling options
- `variations()` - Create variations of generated images
- `inpaint()/outpaint()/pan()` - Image editing operations
- `describe()` - Generate prompts from images
- `blend()` - Combine multiple images
- `faceswap()` - Face swapping functionality
- `fetch()/fetch_many()` - Check task status
- `account()` - Get account information

## Important Notes

- The library uses CommonJS module syntax (`require`/`module.exports`)
- All methods include JSDoc comments documenting parameters
- Error messages are extracted from `error?.response?.data?.errors?.at(0)?.msg`
- The `verbose` flag enables response logging for debugging
- API key is required and validated in the constructor