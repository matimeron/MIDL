Pro Cbeamline, bor = bor

	on_error, 1

	window, 0, xsiz=1280, ysi=384
	Box, [24,54], [-2,8], truasp='ylo', bor=bor
	axis, 24, -2, 0, /xaxis, xstyle=1, charsize=1.5
	axis, 24, 0, 0, yaxis=0, ystyle=1, yminor=2, charsize = 1.5
	plots, [[24,0],[54,0]], thi=2, col=!pcol.red, line=1
	plots, [[24,0],[52.7,0]], thi=2, col=!pcol.red
	foe = Shape_close([[24.5,-0.2],[27.8,-0.55],[31.3,-0.55],[36,-0.95],$
		[36,0.45],[37.5,0.45],[37.5,-0.4],[36,-0.4],$
		[36,1.35],[26.5,1.35],[26.5,0.85],[24.5,0.85]])
	plots, foe, thi = 2, col = !pcol.dblue
	plots, [[28.5,1.35],[30.5,1.35]], thi=2, col=!pcol.white
	plots, [[26.8,1.45],[28.5,1.45]], thi=2, col=!pcol.green
	plots, [[30.5,1.45],[31.5,1.45]], thi=2, col=!pcol.green
	plots, [[33.5,1.35],[34.5,1.35]], thi=2, col=!pcol.white
	plots, [[34.5,1.45],[35.5,1.45]], thi=2, col=!pcol.green

	idb = Shape_close([[47.7,-0.85],[52.7,-1.25],[52.7,3.15],[47.7,3.15],$
		[47.7,-0.65],[45.6,-0.65],[45.6,0.55],[47.7,0.55]])
	plots, idb, thi=2, col=!pcol.dblue
	plots, [[50,3.15],[51.8,3.15]], thi=2, col = !pcol.white
	plots, [[49.1,3.25],[50,3.25]], thi=2, col = !pcol.green
	plots, [[51.8,3.25],[52.7,3.25]], thi=2, col=!pcol.green

	plots, [[52.7,-1.25],[54,-1.4]], thi=2,col=!pcol.dblue, line=2
	plots, [[52.7,3.15],[54,3.15]], thi=2,col=!pcol.dblue, line=2

	Rectan, xli=0.5*[-1,1], yli=0.42*[-1,1], /no_show, shape=mono
	Rectan, xli=1.5*[-1,1], yli=0.32*[-1,1], /no_show, shape=mirror
	Rectan, xli=0.5*[-1,1], yli=0.25*[-1,1], /no_show, shape=shutter
	Rectan, xli=0.45*[-1,1], yli=0.2*[-1,1], /no_show, shape=crl
	
	mono = Shape_trans(mono,0,1,[28.7,-0.05])
	mirror = Shape_trans(mirror,0,1,[32.5,0])
	shutter = Shape_trans(shutter,0,1,[35.2,0])
	crl = Shape_trans(crl,0,1,[46.7,0])
	
	polyfill, mono, col = !pcol.lblue
	plots, mono, thi=2, col=!pcol.blue
	polyfill, mirror, col = !pcol.lblue
	plots, mirror, thi=2, col=!pcol.blue
	polyfill, shutter, col = !pcol.lblue
	plots, shutter, thi=2, col=!pcol.blue
	polyfill, crl, col = !pcol.lblue
	plots, crl, thi=2, col=!pcol.blue

	Labels, [28.7,32.5,35.2,46.7],[0.6,0.6,0.6,0.8],align=0.5,$
	['Mono','Mirrors','Shut.','CRL'], charsize=1.6, col=!pcol.pink

	return
end