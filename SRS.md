# A (Dotty) Gouda Bot

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
maps, similar to how python works.

### Modules

TODO

## Prioritisation of Work

TODO
