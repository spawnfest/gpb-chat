# gpb-chat

### Description
Is small, prototype project written in Erlang and inspired by [MongooseIM](https://github.com/esl/MongooseIM). Project is written for and during [SpawnFest 2019](https://spawnfest.github.io). The project tries to provide the following changes:
 - use [Protocol buffers](https://developers.google.com/protocol-buffers/) instead of XMPP (XML),
 - use web sockets for persistent connections,
 - store config modules in `ets` instead of dynamic modules
 - fewer features and variability in supported backends (implementations) 

### Requirements
 - Erlang OTP 22
 - rebar3
 - make (GNU Make 3.81)
 - protoc (libprotoc 3.9.1) (needed for python profiling example only)
 - Python 3.6.8 (needed for python profiling example only)

### What are `*.porto` files?
Proto files are the files that define data structures and allow to autogenerate code across different programming languages, serialization and deserialization are unified across programming languages. They are claimed to be [smaller an faster than XML.](https://developers.google.com/protocol-buffers/docs/overview#whynotxml)

Thanks to `rebar3_gpb_plugin` the code can be generated for Erlang.
Python profiling tool requires `protoc`. To generate proto fields for python run `make proto_python`. For your convenience python `*.proto` generated files are committed to reposit. 

### How to test it?
`make test_all_with_cover`
Notice that most of the features are not only implemented, but also relarively well tested.
### How to run it?
 - in console run `make shell`
 - open brawser [http://localhost:8765/](http://localhost:8765/) (or run `curl localhost:8765` in second console) you will see minimalistic but encariging `Stayin' Alive` response :)
 - connect to server with web sockets [ws://localhost:8765/user/connect](ws://localhost:8765/user/connect) 

### Comunication
#### Authentication
After connecting a user sends `msg` type `AUTH_TOKEN` to server which reply's with one of:
 - Succesfull authentication -> `HTTP_RESPONSE` with `content` = `"200"` and allows for other type of messages, moreover connection is added to `session table`,
 - Failed authentication -> `HTTP_RESPONSE` with content `"401"` and allows only for `AUTH_TOKEN`.

If a user connects but sends any other `msg` type than `AUTH_TOKEN`, then the server responds with `HTTP_RESPONSE` with content `"401"`.

When a user authenticates successfully, he or she can send `MESSAGE`,
only messages where `from` is equal to users `login` sent in authentication message are allowed and delivered, in any other case server returns `HTTP_RESPONSE` with content `"401"`.

#### Messages
User can send a message to another user knowing his or her login.
Only when both users are connected messages are delivered:
User A sends the `msg` type `MESSAGE`:
```
msg{
    message_type = MESSAGE,
    from = "A",
    to = "B",
    content = "Hello B"
}
```
If user B is not connected (no records in `session table`), than `HTTP_RESPONSE` with content `"401"` is responded. If user B is connected than message is passed to all users B sessions and user A receives `HTTP_RESPONSE` with content `"200"`. When connection is terminated it is removed from `session table`.

#### offline_api

The module allows storing messages for currently not logged in user.
For the example above when user B is offline, user A receives `HTTP_RESPONSE` with content `"201"` and messages stored in server storage. When a user logs, after he authenticates, he is delivered all messages left for him, from server offline storage.
The current implementation is based on `ets` table.

#### Python client

There is a python script added connecting a user and sending messages to yourself.
The script accepts the number of messages to be sent as an argument with the default value set to 1 message.
This is example use and the results:
In the first console (terminal) run:

```
cd $PATH_TO_REPO
make shell
```

In second console (terminal) run:

```
cd $PATH_TO_REPO
python python_client/client.py 4000
```

Results on my Mac are the following:
```
Execution time for all 4000 messages in 0.946868896484375 seconds
Execution time for 1 message is 0.00023671722412109376 seconds
```

#### Replaceable and configurable modules
`auth_api` and `offline_api` modules are both examples of:

You can also easily implement your storage system.
The tests, as well as rest of code, use *proxy* module, which works like interface in OOP.


#### JS client
Requires `npm`.

You can try to chat by yourself with following setup:
 - In console run `make proto_js`
 - Than, in console run `make shell`
 - Open [http://localhost:8765/client](http://localhost:8765/client) in browser
 -  Write a message

Notice that firs message is `AUTH_TOKEN` an all next are `MESSAGE` type.
Browser will switch automatically for that :)

You can see the log messages for sent end received messages.
