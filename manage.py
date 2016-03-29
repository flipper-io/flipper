import sys, os
import shutil

template = os.path.join('examples', 'template')

def replace_and_rename(path, name):

    if os.path.splitext(os.path.basename(path))[0] != 'template':
        return

    if os.path.isdir(path):
        os.rename(path, os.path.join(os.path.dirname(path), name))
        return

    with open(os.path.join(os.path.dirname(path), name + os.path.splitext(path)[1]), 'wt') as output:
        with open(path, 'rt') as input:
            for line in input:
                output.write(line.replace('template', name))

    os.remove(path)

def copy_and_overwrite(source, dest):
    if os.path.exists(dest):
        if raw_input('\nA module with that name already exists. Overwrite? [y/n]: ') == 'y':
            print('')
            shutil.rmtree(dest)
        else:
            print('\nAborting.\n')
            sys.exit()
    shutil.copytree(source, dest)

def create_module(name):
    path = os.path.join('drivers', name)
    # Copy the template.
    copy_and_overwrite(template, path)
    # Overwrite the template with the new module name.
    for root, subdirs, files in os.walk(path):
        for file in files:
            replace_and_rename(os.path.join(root, file), name)
        replace_and_rename(root, name)
    return

def unlink_module(name):
    p = os.path.join('include', name)
    if os.path.exists(p):
        shutil.rmtree(p)
    p = os.path.join('libflipper', 'drivers', name)
    if os.path.exists(p):
        shutil.rmtree(p)
    targets = [os.path.join('osmium', 'targets', s) for s in os.listdir(os.path.join('osmium', 'targets'))]
    for target in targets:
        if os.path.isdir(target):
            p = os.path.join(target, 'drivers', name)
            if os.path.exists(p):
                shutil.rmtree(p)
    return

def force_symlink(source, dest):
    if os.path.lexists(dest):
        os.remove(dest)
    os.symlink(source, dest)

def link_driver(driver, common, dest, intermediate):
    wd = os.getcwd()
    os.chdir(dest)
    base = os.path.join(('..' + os.path.sep) * (dest.count(os.path.sep) + 1), '..')
    if not os.path.exists(driver):
        os.mkdir(driver)
    os.chdir(driver)
    if common != '':
        if not os.path.exists('common'):
            os.mkdir('common')
        os.chdir('common')
        for c in common:
            force_symlink(os.path.join(base, '..', c), os.path.join(os.path.basename(c)))
        os.chdir('..')
    for f in os.listdir(os.path.join(base, intermediate)):
        src = os.path.join(base, intermediate, f)
        force_symlink(src, f)
    os.chdir(wd)

def rebuild_symtree():
    for driver in os.listdir('drivers'):
        path = os.path.join('drivers', driver)
        if (os.path.isdir(path)):
            # Patch the targets into osmium.
            targets = [os.path.join(path, 'targets', s) for s in os.listdir(os.path.join(path, 'targets'))]
            common = [os.path.join(path, 'common', s) for s in os.listdir(os.path.join(path, 'common'))]
            to_libflipper = [os.path.join(path, driver, s) for s in os.listdir(os.path.join(path, driver))]
            for target in targets:
                if os.path.isdir(target) and os.path.basename(target) != 'shared':
                    dest = os.path.join('osmium', 'targets', os.path.basename(target), 'drivers')
                    link_driver(driver, common, dest, target)
                    dest = os.path.join('osmium', 'targets', os.path.basename(target), 'drivers', driver)
                    if os.path.exists(os.path.join(path, 'targets', 'shared')):
                        link_driver('shared', '', dest, os.path.join(path, 'targets', 'shared'))
            dest = os.path.join('libflipper', 'drivers')
            link_driver(driver, common, dest, os.path.join(path, driver))
            link_driver(driver, '', 'include', os.path.join(path, 'include'))
    return

def collapse():
    for driver in os.listdir('drivers'):
        if os.path.isdir(os.path.join('drivers', driver)):
            unlink_module(driver)

def main():

    # Ensure we have the proper number of command line arguments.
    if len(sys.argv) < 2:
        print('\nUsage: manage.py [create | unlink | rebuild]\n')
        return

    if sys.argv[1] != 'rebuild' and sys.argv[1] != 'collapse' and len(sys.argv) < 3:
        print('\nPlease specify the name of the module you would like to ' + sys.argv[1] + '.\n')
        return


    # Parse the command line arguments.
    if sys.argv[1] == 'create':
        create_module(sys.argv[2])
    elif sys.argv[1] == 'unlink':
        unlink_module(sys.argv[2])
    elif sys.argv[1] == 'collapse':
        collapse()
    elif sys.argv[1] == 'rebuild':
        collapse()
        rebuild_symtree()
    else:
        print('\nYou have specified an invalid command.\n')

#main()
print("Update me.")
