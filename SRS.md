# A (Dotty) Gouda Bot

Document Version: 2

## Introduction

### Purpose

This document aims to provide a  description of the requirements for A
Gouda Bot (hencefoth, Gouda). It  will explain the scope, features and
technical details required  for the development of  this software. The
document is intended  as a reference and guide for  the development of
this system.

### Scope

Gouda is a chat bot intended for interacting with the Matrix protocol,
providing participants in  a given room with features  to enrich their
chatting  experience and  also provide  useful functionality  with the
convenience of multiple recipients.

In  some circumstances,  it is  intended  that the  program will  send
ad-hoc messages to  the chat room in order  to stimulate conversation,
much as a human would.

Gouda will be run via a command  line interface, and is intended to be
run  on  a remote  server  or  VPS  for  permanent uptime  given  that
conversation can  occur at any time  and users will expect  a response
regardless of whether there are any other people online.

### Definitions

Gouda: The chat bot.

User: Member of the chat room who will interact with the bot.

TODO

### Overview

The  remainder  of  this  document  will  detail  in  depth  technical
considerations regarding  the development  of the  system, as  well as
high  level features  (at  a level  that an  end  user would  consider
relevant) and a broader level of technical design at a systems level.

## Overall Description

### Language Considerations

The  language of  choice for  this  project will  be Ada  due to  it's
offering  of a  general  purpose, compiled  language  with a  powerful
static type  system. The  aim is  to learn a  compiled language  and I
believe Ada is one of the best available. Previous chat bots have been
implemented in Python which offers  no compilation, no type safety and
poor performance. While  Ada is primarily marketed  for programming of
critical systems, I would like to  explore it's viability in a simpler
context. However it could be said  that we require significant up time
to serve the users of the chat.

Other  languages which  were considered  were Crystal  and Python.  As
mentioned before, Python  is generally a poor choice  for any software
of note,  regardless of what  success other people have  achieved with
it. My  intention is  to have good  performance, high  reliability and
ease  of maintenance  and extension  - none  of which  are offered  by
Python. Crystal  is a more  viable candidate but I  was keen to  use a
language with a  stronger type system than a  gradually typed language
can offer.

Due to the choice  of language, the bot will be  designed in an object
orientated manner.

### Platform Support

This bot is only intended to be run on Linux (and will be developed on
Xubuntu).

### Libraries

A complete or near-complete implementation of the Matrix REST API will
be required. To facilitate this, I  can use two widely used and stable
Ada libraries:

* AWS (Ada Web Server) - a fully functional web library supporting GET
  and POST requests,  as well as https. It offers  many other features
  which may be required further down the line.

* GNATCOLL  (GNAT Component  Collection) -  this provides  many useful
  modules   which  you   would  expect   in  any   modern  programming
  language. The  JSON and SQL  components will absolutely  be required
  for development of Gouda, and the  Strings module may also be useful
  for working with these libraries (and for performance concerns).

### Functionality

Gouda  has always  been,  and  will continue  to  be,  a modular  chat
bot. This means that additional  functionality can be loaded, unloaded
and  reloaded  without   a)  restarting  the  bot,   and  b)  continue
development  of functionality  without interfering  too much  with the
core bot code. Modules during  runtime will be delegated to coroutines
using Ada's tasks.

The compilation and building of modules  can be facilitated by the Ada
build tool GPRBuild.  Modules will be configured as  child projects of
the main bot,  compiled to shared objects and loaded  in at runtime in
the main  bot. Registration of modules  will be done by  searching the
modules directory provided  with the bot, looking  for libraries which
can be loaded.

The  bot will  require  some  form of  CLI  for  ease of  development,
debugging  and maintenance  of  the  bot. Via  the  command line,  the
administrator  will  be able  to  load,  unload and  reload  available
modules,  as   well  as  inspect   log  information  and   change  the
configuration of the bot. The bot will run in a primary thread and the
command line interface will be running in a subsequent thread.

Interaction  with the  bot  will  not primarily  be  done via  bespoke
commands,  but  more as  conversations  in  a natural  human  language
style. Rudimentary ad-hoc natural language  processing will be used as
it has been done in previous iterations of this bot.

## Functional Requirements

### JSON

Given Matrix  is a REST  API, it is  imperative that the  code dealing
with JSON  data is well written.  GNATCOLL offers a JSON  module which
supports creation of a  JSON object, serialisation and deserialisation
to and from strings and building up JSON objects one field at a time.

Some functionality should be provided  for converting to and from hash
maps,  similar to  how python  works. This  will be  implemented as  a
standalone API,  but will still  be part  of the main  codebase. There
should be  a method for converting  a "dictionary" object to  a stdlib
Ada  style hash  map, and  a  function for  converting a  "dictionary"
object to  a string representing  json data.  It may be  necessary for
there to be a method which converts a Map to a json-string but this is
currently unclear.

### Config File

The configuration  file for the  bot will also  be provided as  a json
file  which  should   be  loaded  when  the  bot  starts   up.  If  no
configuration  file is  found, the  bot will  prompt the  user with  a
wizard for creating a basic config.

The following fields are required:

* Username - the username required to connect to the matrix server.
* Password - the password required to connect to the matrix server.
* Address - the address of the server to connect to.
* Default Room - The room to join upon connecting to the server.

### Matrix API

The core  work of  the bot  will be the  implementation of  the Matrix
API. Note that E2EE will not currently be supported.

The  bot should  poll  the server  at 3  second  intervals. It  should
request as little information as  possible from the server during this
polling. Ada's time types should be used for the polling.

The bot should identify users by their id, but address them with their
display name.

Request endpoints have the following form:

/_matrix/client/\<version\>/\<end\>/\<points\>?some=parameters&more=parameters

The bot will need to be able  to easily specify the version of the API
and  whatever  endpoint  it  wishes  to  access.  The  parameters  are
generally optional, however once logged  in, most requests require the
access_token parameter. Due to the  persistence and wide-spread use of
this parameter,  it will  be stored  as an attribute  on the  main bot
client class.

The  Gouda class  must also  be  able to  keep track  of user  account
information provided  by the login  endpoint, such as the  user_id and
device_id.

The following end points will be implemented for version 1.

* unstable/login - connect to the server
* unstable/logout - logout (manual)
* unstable/join - join a room
* unstable/rooms/{..}/leave - leave a room (manual)
* unstable/profile/{..}/avatar_url - update the avatar
* unstable/profile/{..}/displayname - update the display name
* All of the valid "Room Participation" API.
* unstable/presence/{..}/status - update status (last online)
* unstable/rooms/{..}/read_markers - update the read marker position

Instant messaging should support all events and message types.

A separate API  will be used for  the media end points  since they are
sufficiently logically  different to the  client. The urls  start with
the prefix:

/_matrix/media/\<version\>/

* /_matrix/media/unstable/upload - uploading content
* /_matrix/media/unstable/download/{..}/{..}/{..} - download content

These will not necessarily be supported for version 1. OpenCV may also
be utilised  by a  module at some  point. Ada has  good enough  FFI to
achieve this if no libraries are available.

The config  file may take  a new field  "media_hours" which will  be a
whitelist time range of when it is safe to upload media.

### Messaging

Before sending any message to the server, all potential responses from
various modules  or core functionality  will be collated in  a staging
area  where they  will be  prioritised  and/or adapted  based on  some
conditions (not sure what yet).

If  it decides  that multiple  messages  are of  equal importance,  it
should send  them both, with a  slight delay between them  and perhaps
some recognition  of the  fact multiple messages  have been  sent, for
example:

Message 1: I rate this thing a 10/10.
Message 2: Also, blah blah blah.

The also being the important part - the intention is to make it a more
human like interaction.

### Modules

Modules  should possibly  have their  own  json config.  These can  be
stored in a hierachical manner in  a config class, then exposed to the
correct module at run time.

Modules will all  provide a main subprogram which will  be run for all
input messages to determine any meaningful output.

Images  API -  any viable  API similar  to google  images (or  perhaps
google itself) which  provides searching for images based  on a search
term.

YesNo / 8-ball - give answers to basic questions.

Rating - rate things, this is more likely to be a bespoke command.

Quotes  -  save  and   load/show  categorised  quotes,  either  unique
categories or  for a given user.  This will require SQL.  The database
connection will be exposed to all modules from the core bot.

Acronym -  write a string to  the chat where users  can determine what
the acronym might stand for. This will be a bespoke command.

Keywords   -  respond   to  key   words  and   phrases  with   various
responses. The  odds of replying  when a key  phrase is noted  in chat
will be  random so to not  tire the users.  An example may be  where a
user says "back", Gouda will reply "welcome back", although these will
be configured by the user in the module's config file.

Brain -  tbd. Something  like gauging  the atmosphere  in the  room by
detecting certain words  or responses.  This will be  reflected with a
"mood" stored as a vector between a  few common states of mood. It may
be possible to manipulate the response from any module based upon this
mood,  or delay  responses,  or ignore  certain  users (although  some
indication must  be given  that a  user has  been ignored).  The brain
should also  have some capability  of determining how many  people are
online and  talking (look at rate  of flow of messages),  perhaps this
could alter the polling rate or something.

## Prioritisation of Work

1. Implement JSON routines and loading of JSON config.
2. Implement simple REST api routines such as GET, POST and PUT.
3. Implement handler code for specific Matrix endpoints.
4. Implement server polling once syncing is being worked on.
5. Once the bot can send messages  to the server, a basic staging area
   for messages should be written.
6. Implement and delegate all message sending to modules.

### Possible Classes

Gouda -  main bot class,  keeps track  of connection information  in a
similar way that a user would, i.e. know about the password, username,
user_id, access_token. Also  runs the main connection  loop which runs
all of the functionality against the server.

Config -  Config handling for main  bot and modules. This  is owned by
the main bot class.

Connection - Used by the bot class to interact with the Matrix API.

Staging Area - Used by the bot class to manage messages.

Module Control  - Load/Reload/Unload modules  and run all  module code
(via coroutines) and send them to the bot.

Command line - Any CLI functionality required. Separate thread.

Database - Database connection and routines, provided to modules.

JSON - Utility function for JSON handling.

More may be required.

## License

Gouda will be licensed under the GPLv3 license.
