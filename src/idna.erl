-module(idna).

-export([to_ascii/1, to_unicode/1]).

to_ascii(Label) ->
    Labels = re:split(Label, <<"\\.">>, [{return, binary}]),
    io:format("laveks ~p", [Labels]),
    to_ascii(Labels, <<>>).

to_unicode(Label) ->
    Labels = re:split(Label, <<"\\.">>, [{return, binary}]),
    to_unicode(Labels, <<>>).


to_ascii([], Acc) ->
    Acc;
to_ascii([Label|Labels], <<>>) ->
    to_ascii(Labels, idna_nif:to_ascii_nif(Label));
to_ascii([Label|Labels], Acc) ->
    Label1 = idna_nif:to_ascii_nif(Label),
    to_ascii(Labels, << Acc/binary, ".", Label1/binary >>).

to_unicode([], Acc) ->
    Acc;
to_unicode([Label|Labels], <<>>) ->
    to_unicode(Labels, idna_nif:to_unicode_nif(Label));
to_unicode([Label|Labels], Acc) ->
    Label1 = idna_nif:to_unicode_nif(Label),
    to_unicode(Labels, << Acc/binary, ".", Label1/binary >>).
