Pro diatemp, powfile,thick,nlayers,sintet,filter

;Thickness in mm

    rdens = 1.673
    pmult =    1.6021773e-16
    bandw = 1e-3
    compfile = 'c_comp'

    tem = Rascii(powfile)
    e = reform(tem(0,*))
    p = pmult/bandw*reform(tem(1,*))

    mu = abs_coeff(e>0.5,elem='c')
    if n_elements(filter) ne 0 then p = p*FPU_fix(exp(-0.1*filter*mu))

    tem = Rascii(compfile)
    mucsr = Splin_eval(e,Splin_coeffs(tem(0,*),tem(2,*)/tem(1,*)))
    muabr = 1 - mucsr

    dt = 1.*thick/nlayers
    edt = rdens/sintet*dt
    expfac = FPU_fix(exp(-0.1*edt*mu))

    absop = fltarr(nlayers)
    comsc = fltarr(nlayers)

    for i = 0, nlayers-1 do begin
	tem = p*(1 - expfac)
	absop(i) = Integ(e,muabr*tem,/val)	
	comsc(i) = Integ(e,mucsr*tem,/val)
	p = p*expfac
    endfor

    tit = ['Absorbed and scattered power in diamond', $
	    'Layer thickness = ' + string(1e3*dt,form='(f6.1," micron")'),$
	    'Sin(theta) = ' + string(sintet,form='(f6.4)')]
    if n_elements(filter) ne 0 then tit = $
    [tit, 'carbon pre-filter of ' + string(1e3*filter,form='(f6.1," micron")')]

    head = ['Layer #', 'Absorbed power (W)','Scattered power (W)']

    print
    tabulate, 1+indgen(nlayers),absop,comsc, title = tit, header = head

    print
    print
    print, 'Total absorbed power = '+string(total(absop),form='(f6.2," Watt")')
    print, 'Total scattered power= '+string(total(comsc),form='(f6.2," Watt")')
    
    return
end
