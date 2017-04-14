URL_PREFIX=http://localhost:9000

echo 'Ouvrir la session via cookie'
wget --keep-session-cookies --save-cookies cookies.txt \
	${URL_PREFIX}'/authenticate?@id=mailto:michel.cadennes@sens-commun.fr&password=-6096966240312080403'

echo 'Un chauffeur signale sa position'
wget --load-cookies cookies.txt \
	--header='Content-Type: application/json' \
 	--post-file=position.json \
	$URL_PREFIX/position

echo 'Un chauffeur signale sa position'
wget --load-cookies cookies.txt \
	--header='Content-Type: application/json' \
 	--post-file=position2.json \
	$URL_PREFIX/position
