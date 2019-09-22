# gpb-chat

### Description

Is small, prototype project inspired by [MongooseIM](https://github.com/esl/MongooseIM). Project is written for and during [SpawnFest 2019](https://spawnfest.github.io).
Project tries to provide following changes:
 - use [Protocol buffers](https://developers.google.com/protocol-buffers/) instead of XMPP (XML)

### Where are porto files?

Proto files are generated with `make compile`.

### Requirements
 - Erlang OTP version 22
 - rebar3
 - protoc (libprotoc 3.9.1)
 - Python 3.6.8

### How to test?

`make test_all`

### How to run?

`make shell`

### API

#### Live check

##### When works

Stayin' Alive

##### When does not work

curl: (7) Failed to connect to 127.0.0.1 port 8765: Connection refused

#### /user/connect with web sockets

User A sends `AUTH_TOKEN` message to server which reply's with one of:
 - `HTTP_RESPONSE` with content `"200"` and allows for other type of messages, and connection is added to `session table`,
 - `HTTP_RESPONSE` with content `"401"` and allows only for `AUTH_TOKEN`.

When User A authenticates successfully he or she can send `MESSAGE`,
only messages where `from` is equal to users `login` are than allowed.
When `from` is not equal to websocket users `login`, `HTTP_RESPONSE` with content `"401"`.

User can send message to another user knowing his or her login.
Only when both users are connected messages are delivered:

User A sends msg`MESSAGE`
```
msg{
    message_type = MESSAGE,
    from = "A",
    to = "B",
    content = "Hello B"
}
```
If user B is not connected (no records in `session table`), than `HTTP_RESPONSE` with content `"401"` is responded. If user B is connected than message is passed to all users B sessions and user A receives `HTTP_RESPONSE` with content `"200"`.

When connection is terminated it is removed from `session table`.

#### Python client

There is python script added connecting a user and sending messages to yourself.
Script accepts number of messages to be send as an argument with default value set to 1 message.
This is example use and the results:
In first console (terminal) run:

```
cd $PATH_TOREPO
make shell
```

In first console (terminal) run:

```
$ cd $PATH_TOREPO
python python_client/client.py 3500
```

Results on my Mac are following:
```
Execution time for all 3500 messages is 1.023620843887329 seconds
Execution time for 1 message is 0.0002924630982535226 seconds
```
