import sys
import os

def main(package_name, version):

    pdir = 'central/{}'.format(package_name)

    os.system('rm -rf build')
    build_dir = 'build'.format(package_name, version)
    package_dir = 'build/{}/{}'.format(package_name, version)
    lib_dir = '{}/lib'.format(build_dir)
    os.system('mkdir -p {}'.format(package_dir))
    os.system('mkdir -p {}'.format(lib_dir))

    for filename in os.listdir(pdir):
        if '.me' in filename:
            origin = os.path.join(pdir, filename)
            dest = os.path.join(package_dir, filename)
            os.system('cp {} {}'.format(origin, dest))
        elif '.so' in filename:
            origin = os.path.join(pdir, filename)
            dest = os.path.join(build_dir, 'lib', filename)
            os.system('cp {} {}'.format(origin, dest))
        elif '.img' in filename:
            origin = os.path.join(pdir, filename)
            scp = 'scp {} thiago@memetalk.org:/var/www/modules/central/{}/{}'.format(origin, package_name, 'core-{}.img'.format(version))
            print scp
            os.system(scp)

    tar_name = '{}-{}.tar.gz'.format(package_name, version)
    os.system('tar cvfz {} {} {}'.format(tar_name, package_dir, lib_dir))
    scp = 'scp {} publish@memetalk.org:/var/www/modules/central/{}/{}'.format(tar_name, package_name, tar_name)
    print scp
    os.system(scp)

main(sys.argv[1], sys.argv[2])
