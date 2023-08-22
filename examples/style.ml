(*---------------------------------------------------------------------------
   Copyright (c) 2021 The htmlact programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

let base = {css|
/*   Copyright (c) 2021 The htmlact programmers. All rights reserved. */

*, *::before, *::after
{ box-sizing: border-box; margin:0; padding:0;
  background-color: transparent; color: inherit;
  font: inherit; letter-spacing: inherit; word-spacing: inherit;
  line-height: inherit; vertical-align: baseline;
  text-decoration: inherit;  }

:root
{
  --bg: white;
  --fg: black;
  --color-link: blue;
  --color-link-visited: #8e34a5;
  --color-link-hover: #343ca5;
  --redish: 197, 30, 58;
  --greenish: 113, 188, 120;
  --field-bg: #DDD;

  --dur-notice: 100ms;
  --dur-short: 250ms;
  --dur-short-outro: 200ms;
  --dur-medium: 500ms;
  --dur-medium-outro: 400ms;
  --dur-long: 1000ms;
  --dur-long-outro: 750ms;

  --size-body: 1.375rem;
  --size-xx-small: calc(0.4545 * var(--size-body));
  --size-x-small: calc(0.675 * var(--size-body));
  --size-small: calc(0.8181 * var(--size-body));
  --size-large: calc(1.25 * var(--size-body));
  --size-x-large:   calc(1.75 * var(--size-body));

  --size-line-ratio: 1.325;
  --size-line: calc(var(--size-line-ratio) * var(--size-body));
  --size-fourth-line: calc(0.25 * var(--size-line));
  --size-half-line: calc(0.5 * var(--size-line));

  --rule: 1px solid #eee;
}

html { width: 100vw; height: 100vh; }
body { margin: 0 auto;
       background-color: var(--bg); color: var(--fg);
       padding: calc(1.5 * var(--size-line)) var(--size-line);
       max-width: 55ch;
       font-family: sans-serif;
       font-size: var(--size-body);
       line-height: var(--size-line); }

.description
{ border-bottom: 1px solid #eee;
  padding-bottom: var(--size-fourth-line);
  margin-bottom: var(--size-half-line); }


small { font-size: var(--size-small); line-height: 0; }

body > * + *, .description > * + * { margin-top: var(--size-half-line) }

/* Anchors */

a, .link { text-decoration: underline;
    text-decoration-thickness: 2px;
    text-underline-offset: 2px;
    color: var(--color-link); }

a:hover, a:hover .link
{ color: var(--color-link-hover);
  transition: var(--dur-notice) color ease-in-out; }

a:hover:visited, a:hover:visited .link { color: var(--color-link-visited); }

/* Headings */

h1, h2 { font-weight: 700; }
h1 { font-size: var(--size-x-large); margin-bottom: var(--size-line); }
h1 small { font-size: var(--size-x-small); }
h1 a + a { padding-left: 1ex; }
h2 { font-size: var(--size-x-large); font-weight: 700; }

/* Lists */

.examples { list-style: none; counter-reset: item; }
.examples small { padding-left: 1ex; }
.examples a
{ color: var(--fg); text-decoration: none; text-decoration-thickness: 0; }

li + li { margin-top: var(--size-fourth-line) }

/* Tables */

table { border-collapse: collapse; border-spacing: 0; width: 100%; }
td, th { text-align: left; }
th { text-align: left; font-weight: 700;
     border-bottom:1px solid #eee;
     padding-bottom: var(--size-half-line); }

td { text-align: left;
     padding-top: var(--size-fourth-line);
     padding-bottom: var(--size-fourth-line); }
td + td, th + th { padding-left:1.5em; }

/* Form and formable elements */

label { font-weight: 700; }

.field { display: inline-flex; justify-content: space-around; padding: 0.2ex; }

input.field
{ appearance: none;
  background-color: var(--field-bg); border-radius: 0.15rem; border: none;
  text-overflow: ellipsis; }

.button + * { margin-left: 1ex; }
.button
{ appearance: none;
  display: inline;
  position: relative; /* For the spinner overlay */
  min-width: 7ex; padding: 0.25rem 0.5rem; border-radius: 0.15rem;
  border: solid 1px #545454; background-color: #DFDFDF;
  cursor: pointer; user-select: none; }

.button span { pointer-events: none; }

.button:active { background-color: #9cb1c9; }
.button:disabled, .button.htmlact-request
 { border: solid 1px #888; background-color:#AAA; color:#555; }

.htmlact-request .spinner + span { filter: blur(0.5px); opacity: 0.5; }

.button .spinner
{ visibility: hidden;
  width: calc(0.9 * var(--size-line)); height: calc(0.9 * var(--size-line));
  /* Centering */
  position: absolute; left: 0; right: 0; top:0; bottom: 0; margin: auto;
  border: solid 3px white; border-bottom-color: black; border-radius: 50%;
  opacity: 0.5; }

.htmlact-request .spinner
{ animation: spin var(--dur-long) linear var(--dur-long) infinite; }

@keyframes spin
{ from { visibility: visible; } to { transform: rotate(360deg); }}
|css}
