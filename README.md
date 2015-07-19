Erlang Filesystem Listener
===========

Backends
--------

* Mac [fsevent](https://github.com/thibaudgg/rb-fsevent)
* Linux [inotify](https://github.com/rvoicilas/inotify-tools/wiki)
* Windows [inotify-win](https://github.com/thekid/inotify-win)

NOTE: On Linux you need to install inotify-tools.

### Subscribe to Notifications

```erlang
> enotify:start_link("./").
> flush().
Shell got {".../enotify/src/README.md",[closed,modified]}}
```

Credits
-------

* Vladimir Kirillov
* Maxim Sokhatsky

OM A HUM
