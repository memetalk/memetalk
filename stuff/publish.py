import sys
import os

def main(package_name, version):

    pdir = 'central/{}'.format(package_name)

    build_dir = 'build'
    os.system('rm -rf {}'.format(build_dir))

    package_dir = 'build/cache/{}/{}'.format(package_name, version)
    lib_dir = '{}/lib'.format(build_dir)
    os.system('mkdir -p {}'.format(package_dir))
    os.system('mkdir -p {}'.format(lib_dir))

    for filename in os.listdir(pdir):
        if '.me' in filename or '.g' in filename or '.k' in filename:
            origin = os.path.join(pdir, filename)
            dest = os.path.join(package_dir, filename)
            os.system('cp {} {}'.format(origin, dest))
        elif '.so' in filename:
            origin = os.path.join(pdir, filename)
            dest = os.path.join(build_dir, 'lib', filename)
            os.system('cp {} {}'.format(origin, dest))
        elif '.img' in filename:
            origin = os.path.join(pdir, filename)
            scp = 'scp {} publish@memetalk.org:/var/www/modules/central/{}/{}'.format(origin, package_name, 'core-{}.img'.format(version))
            print scp
            os.system(scp)

    tar_name = '{}-{}.tar.gz'.format(package_name, version)
    os.system('cd {}; tar cvfz {} cache lib'.format(build_dir, tar_name))
    scp = 'cd {}; scp {} publish@memetalk.org:/var/www/modules/central/{}/{}'.format(build_dir, tar_name, package_name, tar_name)
    print scp
    print os.system(scp)

main(sys.argv[1], sys.argv[2])
