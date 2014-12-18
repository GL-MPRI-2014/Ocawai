sleep 5
xdotool search --name OCAWAI key Up
xdotool search --name OCAWAI key Up
xdotool search --name OCAWAI key Down
xdotool search --name OCAWAI key Down
xdotool search --name OCAWAI key Left
xdotool search --name OCAWAI key Right
xdotool search --name OCAWAI key Left
xdotool search --name OCAWAI key Right
xdotool search --name OCAWAI key b
xdotool search --name OCAWAI key a
sleep 3
xdotool search --name OCAWAI key space
sleep 3
xdotool search --name OCAWAI key q
sleep 2
xdotool search --name OCAWAI key Up
xdotool search --name OCAWAI key Return
sleep 3

for i in {1..50}
do
	xdotool search --name OCAWAI key Up
done
for i in {1..100}
do
	xdotool search --name OCAWAI key Down
done
for i in {1..100}
do
	xdotool search --name OCAWAI key Left
done
for i in {1..100}
do
	xdotool search --name OCAWAI key Right
done

sleep 1
xdotool search --name OCAWAI key Return
xdotool search --name OCAWAI key Down
xdotool search --name OCAWAI key Return
xdotool search --name OCAWAI key Left
xdotool search --name OCAWAI key Return
sleep 1
xdotool search --name OCAWAI key Down
xdotool search --name OCAWAI key Left
xdotool search --name OCAWAI key Return
sleep 1
xdotool search --name OCAWAI key ctrl+c &> /dev/null || true
