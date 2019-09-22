-module(msg_helper).

-compile(export_all).

fresh_login() ->
    Bin = crypto:strong_rand_bytes(10),
    binary_to_list(Bin).
