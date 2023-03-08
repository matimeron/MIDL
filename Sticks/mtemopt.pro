Pro MTemopt, border = bor

	on_error, 1

	Box, [-1,1], [-0.3,2], truasp = 'ylo', xmargin = [4,4], border=bor

	r = 4.
	z = Make_grid([-0.6,0.6],0.005,/step)
	y = z^2/(2*r)

	tet = 30.
	rtet = !dtor*tet
	dx = 0.3
	zc = (Polsolve([dx/cos(rtet),-tan(rtet),-1./(2*r)]))[1]
	yc = zc^2/(2*r)

	plots, [-1,0], [1*tan(rtet),0], line=2, thi=3, col=!pcol.dgreen
	plots, [-1,zc],[1*tan(rtet)+dx/cos(rtet),yc], $
		line=2, thi=3,col=!pcol.dgreen
	plots, [0,1], [0,1*tan(rtet)], line=2, thi=3, col=!pcol.dgreen
	plots, [zc,1],[yc,yc + [1-zc]*tan(rtet + 2*zc/r)], $
		line=2, thi=3,col=!pcol.dgreen
	plots, z, y, thi=4, col=!pcol.blue

	Arc_mm, [-0.8,0.8*tan(rtet)],cent=[0,0],ang=rtet, thi=2,line=1
	Arro, from = [0,-0.1], to = [0.3,-0.1]
	Arro, from = [0,0.5], to = [0 + 0.3*sin(rtet),0.5+0.3*cos(rtet)]

	Labels, [-0.7,0.12,0.15], [0.2,0.6,-0.17], align=0.5, charsize=2, charthi=2, $
	['!7h!x','x','z']

	return
end