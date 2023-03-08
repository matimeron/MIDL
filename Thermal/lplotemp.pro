Pro Lplotemp, d, t, ptemp, select=sel, opselect = opsel, _extra = _e

    nd = n_elements(d)
    sel = Default(sel,lindgen(nd))
    opsel = Default(opsel,lindgen(nd))
    nd = n_elements(sel)

    plos = ptemp[[sel],*]
    labs = string(d[sel],form='(f4.1)')
    xran = [min(t,max=xmax),10*xmax]

    plotot, t, plos, xrange=xran, yrange=[0,4], line=lonarr(nd), $
    title = 'Form function for line power load !c' + $
    'surface heating, back cooling', xtit = 't/!12l!x!dB!N', $
    ytit = 'F(d/!12l!x!dB!N, t/!12l!x!dB!N)', charsize=1.2, charthick=1.4, $
    subtit = 'The thick line passes through the minima of the curves', $
    _extra = _e

    xl = (xran(1)/2)[0]
    yl = 3.2 - 0.18*findgen(n_elements(sel))
    ylt = yl[0] + 0.2
    tips = reform(ptemp([sel],n_elements(t)-1))

    labels, xl, ylt, 'd/!12l!x!dB!N', align = 0.5, charsize=1.2, charthick=1.4
    labels, xl, yl, labs,align=0.5

    xl = xl[0]
    for i = 0, nd-1 do $
	arro, from = [xl-8,yl[i]+0.02],to = [10.4,tips[i]-0.01+0.002*i],siz=.3

;    wd = Cast(d[opsel],4,4)
;    wt = Cast(t,4,4)
;    wtemp = Cast(ptemp[opsel,*],4,4)
    wd = d[opsel]
    wt = t
    wtemp = ptemp[opsel,*]
    nwd = n_elements(wd)
    opd = dblarr(nwd)
    opt = opd
    for i = 0, nwd - 1 do begin
	spc = splin_coeffs(wt,wtemp(i,*))
	tder = splin_eval(wt,spc,deriv=1)
	dspc = splin_coeffs(wt,tder)
	opd[i] = splinroot(dspc,[.12,.50],1e-8, stat=stat)
	if stat[0] ne 1 then opd[i] = 0
	opt[i] = splin_eval(opd[i],spc)
    endfor
    dum = where(opd ne 0)
    opd = opd(dum)
    opt = opt(dum)

    lopd = [0.457,opd,0.124]
    lopt = [3.973,opt,1.159]

;    oplot, opd, opt, thick=3
    oplot, lopd, lopt, thick=3

    return
end
