"""Equal-probability contours for two independent unbiased vector estimators."""
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import Ellipse, FancyArrowPatch
import numpy as np

OUT = Path(__file__).resolve().parent
S1 = np.diag([4.0, 1.0])
S2 = np.diag([1.0, 4.0])
SC = np.linalg.inv(np.linalg.inv(S1) + np.linalg.inv(S2))
LEVEL = 5.991464547107979  # 95th percentile of chi-squared with two df
BLUE, ORANGE, INK, GRAY = "#287B9B", "#C37B38", "#263D50", "#A6ADB0"
plt.rcParams.update({"font.family": "DejaVu Sans", "svg.fonttype": "none"})
fig = plt.figure(figsize=(12, 6), facecolor="none")
axes = [fig.add_axes([.04, .18, .40, .68]), fig.add_axes([.56, .18, .40, .68])]

def contour(ax, cov, color, alpha=.12, lw=2.8, linestyle="-"):
    vals, vecs = np.linalg.eigh(cov)
    angle = np.degrees(np.arctan2(vecs[1, 1], vecs[0, 1]))
    wh = 2 * np.sqrt(LEVEL * vals[::-1])
    ax.add_patch(Ellipse((0, 0), *wh, angle=angle, facecolor=color,
                        edgecolor="none", alpha=alpha))
    ax.add_patch(Ellipse((0, 0), *wh, angle=angle, facecolor="none",
                        edgecolor=color, linewidth=lw, linestyle=linestyle))

for ax in axes:
    ax.set(xlim=(-5.6, 5.6), ylim=(-5.6, 5.6), aspect="equal")
    ax.set_axis_off()
    ax.plot([-5.25, 5.25], [0, 0], color=GRAY, lw=.8, alpha=.65, zorder=0)
    ax.plot([0, 0], [-5.25, 5.25], color=GRAY, lw=.8, alpha=.65, zorder=0)
    ax.text(5.4, -.45, r"$\hat\theta_1$", color=INK, fontsize=15, ha="center")
    ax.text(.4, 5.35, r"$\hat\theta_2$", color=INK, fontsize=15, va="center")
    ax.scatter([0], [0], s=32, c=INK, zorder=6)

contour(axes[0], S1, BLUE)
contour(axes[0], S2, ORANGE)
contour(axes[1], S1, "#C9D3D6", alpha=0, lw=1.2, linestyle=(0, (4, 4)))
contour(axes[1], S2, "#C9D3D6", alpha=0, lw=1.2, linestyle=(0, (4, 4)))
contour(axes[1], SC, INK, alpha=.16, lw=3.2)

fig.text(.24, .91, "Two estimators", ha="center", color=INK, fontsize=23)
fig.text(.76, .91, "Optimal combination", ha="center", color=INK, fontsize=23)
fig.add_artist(FancyArrowPatch((.455, .52), (.545, .52), transform=fig.transFigure,
                             arrowstyle="-|>", mutation_scale=23, lw=2, color=INK))
fig.text(.5, .62, "Matrix\nweights", ha="center", va="center", color=INK, fontsize=14)
fig.text(.145, .105, "Estimator 1", ha="center", color=BLUE, fontsize=15)
fig.text(.335, .105, "Estimator 2", ha="center", color=ORANGE, fontsize=15)
fig.text(.76, .105, "Smaller variance in every direction", ha="center", color=INK, fontsize=15)
fig.savefig(OUT / "combining-estimators.png", dpi=180, transparent=True)
fig.savefig(OUT / "combining-estimators.svg", transparent=True)
plt.close(fig)
