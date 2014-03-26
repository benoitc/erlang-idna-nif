# idna-erlang

idna implements IDNA2008 (Internationalized Domain Names for
Applications), defined in RFC 5890, RFC 5891, RFC 5892, RFC 5893 and RFC
5894.

The current implementation provides a nif binding the [ICU
IDNA](http://icu-project.org/apiref/icu4c/uidna_8h.html) api.

## Usage

    1> A = idna:to_ascii(<<"www.詹姆斯.com"/utf8>>).
    <<"www.xn--8ws00zhy3a.com">>
    2> idna:to_unicode(A).
    <<"www.詹姆斯.com"/utf8>>
