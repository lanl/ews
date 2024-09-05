
x_showmatrix -in=vp.bin -n1=150 -d1=0.01 -d2=0.01 -label1='Depth (km)' -label2='Horizontal Position (km)' -tick1d=0.5 -mtick1=4 -tick2d=0.5 -mtick2=4 -out=vp.pdf -legend=y -unit='P-wave velocity (m/s)' &

x_showmatrix -in=result/all_sensitivity_p_wrt_vp_src_1.bin -n1=150 -d1=0.01 -d2=0.01 -label1='Depth (km)' -label2='Horizontal Position (km)' -tick1d=0.5 -mtick1=4 -tick2d=0.5 -mtick2=4 -cmin=0 -cmax=5000 -out=pvp.pdf &
