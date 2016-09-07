# Platforms

Each platform contains hardware level or platform level implementation needed by the toolbox.

### Compilation

Each platform, when compiled, with yield a dynamically linkable file `lf-platform-NAME` where `NAME` is the name of the compiled platform. This platform file can then be linked against other components of the toolbox to yeild firmware, an application, or a module for the given platform given different compilation strategies.