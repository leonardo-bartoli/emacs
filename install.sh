#!/bin/sh

echo -e '\e[0;32m*** installing emacs configuration ***\e[0;37m'

echo -ne 'coping .emacs...    '
cp ./emacs.el ~/.emacs
sleep 1
echo -e '\e[0;32m [DONE]\e[0;37m'

echo -ne 'coping .emacs.d...  '
cp -R ./emacs.d ./.emacs.d
sleep 1
echo -e '\e[0;32m [DONE]\e[0;37m'


