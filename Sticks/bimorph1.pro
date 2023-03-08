Pro bimorph1

	common bimorph_dat, bexs, nseg, modln, bords, cwei, bmconst
	on_error, 1
	bimorph_init

	box, [-300,300], [-90,300], truasp='ylo', xmar =[5,5], ymar =[2,2], $
		title = 'Bimorph Mirror side view'
	for i = 0, nseg-1 do begin
		Rectan, xlim=bords[i:i+1], ylim = [-50,0], thi=2, col=!pcol.lblue
		Rectan, xlim=bords[i:i+1], ylim = [0,50], thi=2, col=!pcol.lblue
		Rectan, xlim=bords[i:i+1]+[5,-5],ylim=[-2,2],/fill,col=!pcol.red
	endfor
	Rectan, xlim=bords[[0,nseg]],ylim=[-60,-50],/fill, col=!pcol.yellow
	Rectan, xlim=bords[[0,nseg]],ylim=[-60,-50],thi=2, col=!pcol.dblue
	Rectan, xlim=bords[[0,nseg]],ylim=[50,60],/fill, col=!pcol.yellow
	Rectan, xlim=bords[[0,nseg]],ylim=[50,60],thi=2, col=!pcol.dblue
	Rectan, xlim=bords[[0,nseg]],ylim=[-52,-48],/fill, col=!pcol.green
	Rectan, xlim=bords[[0,nseg]],ylim=[48,52],/fill, col=!pcol.green
	Arro, from=[-120,-80], to=[-140,-55],thi=2,siz=1.2
	Arro, from=[-100,-80], to=[-90,55],thi=2,siz=1.2
	Arro, from=[100,-80], to=[100,-25],thi=2,siz=1.2
	Arro, from=[120,-80], to=[130,25],thi=2,siz=1.2
	Arro, from=[bords[0],80], to = [bords[nseg],80], thi=2, /two,size=1.5
	Labels, [-110,110,0],[-95,-95,90],['fused silica','PZT','600 mm'],align=0.5
	Legend_mm, line=0, thi=2, col=[!pcol.red,!pcol.green],$
		text = ['Driving electrodes','Ground electrodes']
	return
end