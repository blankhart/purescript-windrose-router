// Lone surrogates have no UTF-8 encoding, so `encodeURIComponent` throws on
// them.  Browsers replace them with U+FFFD when serializing a URL; do the same.
const loneSurrogate = /[\uD800-\uDBFF](?![\uDC00-\uDFFF])|(?<![\uD800-\uDBFF])[\uDC00-\uDFFF]/g;

export const percentEncode = (s) =>
  encodeURIComponent(s.replace(loneSurrogate, "�"));

// A maximal run of well-formed escapes.  A `%` not followed by two hex
// digits is not matched and so stays literal.
const escapes = /(?:%[0-9A-Fa-f]{2})+/g;

// Replaces invalid UTF-8 with U+FFFD and keeps a leading BOM, as the
// WHATWG URL standard's "UTF-8 decode without BOM" does.
const utf8 = new TextDecoder("utf-8", { ignoreBOM: true });

export const percentDecode = (s) =>
  s.replace(escapes, (run) => {
    const bytes = new Uint8Array(run.length / 3);
    for (let i = 0; i < bytes.length; i++) {
      bytes[i] = parseInt(run.slice(3 * i + 1, 3 * i + 3), 16);
    }
    return utf8.decode(bytes);
  });
