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

