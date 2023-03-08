Pro Wigwin, e, ec, k, gap= gap, npo = np, win = win, beth = bth, cath = cth,$
    surf = sur, outpower = opow

    ftp = 1.6021773e-16

    cur = 0.1
    dist = 25
    rgam = 13700
    nper = 28
    ban = 1e-3

    cth = Default(cth,0.,/dtyp)
    ab = abs_coeff(e,elem='be')
    ac = abs_coeff(e,elem='c')

    pab = FPU_fix(exp(-0.1*cth*ac)*(1 - exp(-0.1*bth*ab)))

    wout = Fwiggler(e,cur,dist,ec,k,rgam,nper,$
			npo=np,win=win,ban=ban,xvals=xvl,yvals=yvl)

    arel = (xvl[2*np[0]] - xvl[0])*(yvl[2*np[1]] - yvl[0])/(4.*np[0]*np[1])

    for i = 0l, n_elements(e)-1 do wout[i,*,*] = pab[i]*wout[i,*,*]
    pdis = make_array(2*np[0]+1,2*np[1]+1,/float)
    for jx = 0l, 2*np[0] do begin
	for jy = 0l, 2*np[1] do begin
	    pdis[jx,jy] = Integ(e,wout(*,jx,jy),/val)
	endfor 
    endfor

    pdm = max(pdis)
    pow = arel*Partot(pdis,symfringe=[-0.5,-0.5])

    tit =  'Power in Be window.  Wiggler A, ' + $ 
	string(gap,form="(i2,' mm gap ;')") + $
	'  K = ' + string(k,form='(f5.2)') + $
	',  E!dc!n = '+string(ec,form="(f5.2,' keV')")
    subtit = 'Total power = ' + string(pow,form='(f6.2," W;  ")') + $
	'Peak power density = ' + string(pdm,form='(f6.3,"  W/mm!e2!n.!c")')+ $
	string(bth,form='(f5.2," mm Be")') + ' at ' + $
	string(dist,form='(f5.2," m .")')
    if cth gt 0 then $
    subtit = subtit + ' ' + string(cth,form='(f5.2," mm C prefilter.")')
    xtit = 'X (mm)'
    ytit = 'Y (mm)'

;    levs = [0.01,0.02,0.05,0.1,0.2,0.5,1,2,5,10,50,100]
;    labs = fix(1 + 0*levs)
    labs = replicate(1,30)

    contour, pdis, xvl, yvl, c_lab = labs, /follow, $
    tit = tit, subtit = subtit, xtit = xtit, ytit = ytit

    if keyword_set(sur) then surface, pdis, xvl, yvl, $
	tit = tit, subtit = subtit, xtit = xtit, ytit = ytit

    dum = Winframe(xvl, yvl, xygrid = xyvl)
    opow = make_array(3, n_elements(xvl), n_elements(yvl), /float)
    opow(0,*,*) = pdis
    opow(1:2,*,*) = xyvl

    return
end
