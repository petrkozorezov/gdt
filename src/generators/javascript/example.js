// require msgpack
// require jQuery
// require foo

var Test = {};

Test.encode = function(format, value) {
    if (format=='json') {
        return $.toJSON(encode_json(value));
    } else if(format=='msgpack') {
        return msgpack.pack(encode_msgpack(value));
    }
    throw "unknown format " + format;
}

Test.decode = function(format, data) {
    if (format=='json') {
        return decode_json($.evalJSON(data));
    } else if(format=='msgpack') {
        return decode_msgpack(msgpack.unpack(data));
    }
    throw "unknown format " + format;
}

Test.encode_ = function(format, value) {
    if (format=='json') {
        return encode_json(value);
    } else if(format=='msgpack') {
        return encode_msgpack(value);
    }
    throw "unknown format " + format;
}

Test.decode_ = function(format, data) {
    if (format=='json') {
        return decode_json(data);
    } else if(format=='msgpack') {
        return decode_msgpack(data);
    }
    throw "unknown format " + format;
}

