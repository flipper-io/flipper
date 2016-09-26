import os, platform

APPNAME = 'flipper-toolbox'
VERSION = '0.1'

top = '.'
out = 'build'

def _platforms(ctx):
    for platform in ctx.env.PLATFORMS:
        ctx.recurse(platform)

def options(opt):
    opt.add_option('--upload', action='store', default='', help='Upload the specified firmware to its target device.')
    # Load the system's C compiler.
    opt.load('compiler_c')

def configure(cfg):
    cfg.env.append_value('INCLUDES', [cfg.path.abspath() + '/include', './include'])
    cfg.env.append_value('INCLUDES', map(str, cfg.path.ant_glob('modules/*/include', dir=True)))
    cfg.env.append_value('INCLUDES', map(str, cfg.path.ant_glob('platforms/*/include', dir=True)))
    # Disable markers during library compilation.
    cfg.env.STLIB_MARKER = []
    cfg.env.SHLIB_MARKER = []
    # Obtain platforms.
    cfg.env.PLATFORMS = map(str, cfg.path.ant_glob('platforms/*', dir=True, src=False))
    # Create platform environments.
    for platform in [os.path.basename(platform) for platform in cfg.env.PLATFORMS]:
        _env = cfg.env.derive()
        _env.detach()
        cfg.setenv('platform-' + platform, _env)
    # Configure all supported platforms.
    _platforms(cfg)

def build(bld):
    # Build all of the standard modules.
    bld.recurse('modules')
    # Build all of the supported platforms.
    _platforms(bld)
    # Build Osmium.
    bld.recurse('osmium')
    # Build libflipper.
    bld.recurse('libflipper')
    # Build the tests.
    bld.recurse('tests')
    # Install the top-level headers.
    bld.install_files('${PREFIX}/include/', bld.path.ant_glob('include/flipper.h'))
    bld.install_files('${PREFIX}/include/flipper/', bld.path.ant_glob('include/flipper/**'))
    if (platform.system() == 'Linux'):
        bld.install_files('/etc/udev/rules.d', 'assets/99-flipper.rules')
