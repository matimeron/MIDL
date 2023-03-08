Pro bimorph2, rad

	common bimorph_dat, bexs, nseg, modln, bords, cwei, bmconst
	on_error, 1
	bimorph_init

	Box, [-1,1], [-0.2,1], truasp='ylo', xmar =[5,5], ymar =[2,2]
	Rectan, xli=[-1,1], yli=[-0.1,0.1], thi=2, col=!pcol.blue
	Rectan, xli=[-1,1], yli=[-0.1,0.1]+0.2, thi=2, col=!pcol.blue
	
	del=0.03
	Arc_mm, [-1-del,0.8], [1+del,0.8], rad=rad, thi=2, col=!pcol.blue
	Arc_mm, [-1,1], [1,1], rad=rad, thi=2, col=!pcol.blue
	Arc_mm, [-1+del,1.2], [1-del,1.2], rad=rad, thi=2, col=!pcol.blue
	Plots, [-1-del,-1+del],[0.8,1.2],thi=2, col=!pcol.blue
	Plots, [1+del,1-del],[0.8,1.2],thi=2, col=!pcol.blue
	Arro, from=[0,0.35], to=[0,0.65],thi=2,siz=2
	Labels, 0.05,0.45, 'V', charsize=2, charthi=2

	return
end