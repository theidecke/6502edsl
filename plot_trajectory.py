#!/usr/bin/env python3
# /// script
# dependencies = ["matplotlib"]
# ///
"""3D line plot of the Lorenz attractor trajectory from attraktor-trajectory.txt."""

import matplotlib.pyplot as plt

xs, ys, zs = [], [], []
with open("attraktor-trajectory.txt") as f:
    next(f)  # skip header
    for line in f:
        parts = line.split()
        if len(parts) >= 5:
            xs.append(float(parts[2]))
            ys.append(float(parts[3]))
            zs.append(float(parts[4]))

fig = plt.figure()
ax = fig.add_subplot(111, projection="3d")
ax.plot(xs, ys, zs, lw=0.5)
ax.set_xlabel("X")
ax.set_ylabel("Y")
ax.set_zlabel("Z")
ax.set_title("Lorenz Attractor (6502 emulator)")
plt.tight_layout()
plt.savefig("attraktor-trajectory.png", dpi=200)
print("Wrote attraktor-trajectory.png")
