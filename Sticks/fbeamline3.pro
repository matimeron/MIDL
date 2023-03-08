Pro Fbeamline3, bor = bor

	on_error, 1

	cang = 5e-4
	off = 1.0

	inb = [1,-cang]
	oub = [1,cang]
	vof = [0,off]
	mof01 = [0,1]##vof
	mof11 = [1,1]##vof

	window, 1, xsiz=1280, ysi=384
	Box, [24,54], [-2,8], truasp='ylo', bor=bor
	axis, 24, -2, 0, /xaxis, xstyle=1, charsize=1.5
	axis, 24, 0, 0, yaxis=0, ystyle=1, yminor=2, charsize = 1.5
	plots, [24,28]##inb, thi=2, col = !pcol.purple
	plots, [24,54]##oub, thi=2, col = !pcol.red, line=1
	plots, [24,52.7]##oub, thi=2, col = !pcol.red

	plots,[30.0,30.0]##inb + mof01, thi=2, col=!pcol.purple, line=2
	plots,[30.0,35.]##inb + mof01, thi=2, col=!pcol.purple, line=2
	plots,[35.3,51.2]##inb + mof11, thi=2, col=!pcol.purple, line=2
	plots,[33.3,46.2]##inb + mof11, thi=2, col=!pcol.purple

	plots, [[38.5,0],[54,-1.37]], thi=2, col=!pcol.cyan, line=2

	foe = Shape_close([[24.5,-0.2],[27.8,-0.55],[31.3,-0.55],[36,-0.95],$
		[36,-0.5],[37.2,-0.5],[37.2,0.5],[38.3,0.5],[38.3,2.1],$
		[36,2.1],[26.5,2.1],[26.5,0.85],[24.5,0.85]])
	plots, foe, thi = 2, col = !pcol.dblue
	plots, [[27.5,2.1],[30.5,2.1]], thi=2, col=!pcol.white
	plots, [[26.6,2.2],[28.6,2.2]], thi=2, col=!pcol.green
	plots, [[30.5,2.2],[31.5,2.2]], thi=2, col=!pcol.green
	plots, [[33.5,2.1],[34.5,2.1]], thi=2, col=!pcol.white
	plots, [[34.5,2.2],[35.5,2.2]], thi=2, col=!pcol.green

	idb = [[52.7,-1.25],[52.7,4.5],[46.7,4.5],$
		[46.7,0.45],[46.7,4.5],[40.7,4.5],[40.7,-0.2],[52.7,-1.25]]
	plots, idb, thi=2, col=!pcol.dblue
	plots, [[42.7,4.5],[44.7,4.5]], thi=2, col = !pcol.white
	plots, [[41.7,4.6],[42.7,4.6]], thi=2, col = !pcol.green
	plots, [[44.7,4.6],[45.7,4.6]], thi=2, col=!pcol.green
	plots, [[48.7,4.5],[50.7,4.5]], thi=2, col = !pcol.white
	plots, [[47.7,4.6],[48.7,4.6]], thi=2, col = !pcol.green
	plots, [[50.7,4.6],[51.7,4.6]], thi=2, col=!pcol.green

	soe = [[48.2,-0.85],[48.2,0.45],[45.3,0.45],[45.3,-0.6]]
	plots, soe, thi=2, col=!pcol.blue
	plots, [[46.2,-0.68],[47.2,-0.77]], thi=2, col=!pcol.white
	plots, [[45.2,-0.69],[46.2,-0.78]], thi=2, col=!pcol.green

	plots, [[52.7,-1.25],[54,-1.4]], thi=2,col=!pcol.dblue, line=2
	plots, [[52.7,3.2],[54,3.2]], thi=2,col=!pcol.dblue, line=2

	Rectan, xli=0.5*[-1,1], yli=0.42*[-1,1], /no_show, shape=mono
	Rectan, xli=0.7*[-1,1], yli=0.32*[-1,1], /no_show, shape=mirror
;	Rectan, xli=0.7*[-1,1], yli=0.42*[-1,1], /no_show, shape=mirror
	Circle_mm, cent=[0,0],rad=0.2,/no_show, shape=scr
	Rectan, xli=2.8*[-1,1], yli= 0.32*[-1,1], /no_show, shape=sbm
	Rectan, xli=0.5*[-1,1], yli=0.25*[-1,1], /no_show, shape=shutter
	Rectan, xli=0.45*[-1,1], yli=0.2*[-1,1], /no_show, shape=crl

	mono = Shape_trans(mono,0,1,28.5*oub-[0,0.05])
	mirror = Shape_trans(mirror,0,1,36.2*oub)
	scr1 = Shape_trans(scr,0,1,30.*inb)
	sbm = Shape_trans(sbm,0,1,32.5*inb+vof)
	shutter1 = Shape_trans(shutter,0,1,47.5*oub)
	shutter2 = Shape_trans(shutter,0,1,37.6*inb+vof)
	crl1 = Shape_trans(crl,0,1,46*oub)
	crl2 = Shape_trans(crl,0,1,36.2*inb+vof)

	polyfill, mono, col = !pcol.lblue
	plots, mono, thi=2, col=!pcol.blue
	polyfill, mirror, col = !pcol.lblue
	plots, mirror, thi=2, col=!pcol.blue
	polyfill, scr1, col = !pcol.lblue
	plots, scr1, thi=2, col=!pcol.blue
	polyfill, sbm, col = !pcol.lblue
	plots, sbm, thi=2, col=!pcol.blue
	polyfill, shutter1, col = !pcol.lblue
	plots, shutter1, thi=2, col=!pcol.blue
	polyfill, shutter2, col = !pcol.lblue
	plots, shutter2, thi=2, col=!pcol.blue
	polyfill, crl1, col = !pcol.lblue
	plots, crl1, thi=2, col=!pcol.blue
	polyfill, crl2, col = !pcol.lblue
	plots, crl2, thi=2, col=!pcol.blue

	Labels, [28.5,36.2,47.5,46],[-1.3,-1.4,-0.6,-0.6],align=0.5,$
		['Mono','Mirrors','Shut.','CRL'],charsize=1.6,col=!pcol.pink
	Labels, [32.5,37.6,36.1],off + [0.4,0.4,0.4],align=0.5,$
		['Sidebounce Mono','Shut.','CRL'],charsize=1.6,col=!pcol.pink

	return
end