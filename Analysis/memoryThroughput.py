import math
import argparse
from dataclasses import dataclass
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter


@dataclass
class Board:
    name: str
    pcie_throughput: float
    local_memory: float
    freq: float
    plot_symbol: str

    def __init__(self, name, pcie_throughput, local_memory, freq, plot_symbol):
        self.name = name
        self.pcie_throughput = pcie_throughput * 8
        self.local_memory = local_memory * 8
        self.freq = freq
        self.plot_symbol = plot_symbol


EARTH_AREA_KM2 = 510_072_000
ASIA_AREA_KM2 = 44_570_000
EUROPE_AREA_KM2 = 10_186_000
SWEDEN_AREA_KM2 = 447_425
GOTLAND_AREA_KM2 = 2_994
AVERAGE_ANIMAL_PER_CELL = 0.25
DEFAULT_ANIMAL_SIZE = 128
DEFAULT_CELL_SIZE = 32

MIN_PIPELINES = 100
MAX_PIPELINES = 500

HALO_STEPS = np.asarray([0, 1, 4, 10, 20, 40, 60, 80, 100, 200])

COMET_065B = Board("Comet A65", 8e9, 16e9, 300e6, "*")
DE10 = Board("DE10-Agilex", 64e9, 32e9, 700e6, "s")
HBM2E_STARTER = Board("HBM2e Dev-kit", 128e9, 48e9, 850e6, "D")
# One DDR4 socket is shared with HPS, so make space for 4Gb for that
MERCURY_A2700 = Board("Mercury A2700 Accelerator", 128e9, 32e9 * 4 - 4e9, 700e6, "v")
AGILEX7_STARTER = Board("Agilex 7 Starter Kit", 16e9, 16e9, 700e6, "^")


def memory_per_s(clk_freq, pipelines, animal_size, cell_size):
    """The memory throughput per second for the given nr of pipelines."""
    # Assume the calculation of one animal tick correspond to the average nr of cell ticks
    # being done too. This only holds if cell updates are very cheap
    return pipelines * clk_freq * (animal_size + cell_size / AVERAGE_ANIMAL_PER_CELL)


def show_fpga_througput(pipelines, animal_sizes):

    fig, ax = plt.subplots()
    for animal_size in animal_sizes:
        ax.plot(
            pipelines,
            memory_per_s(COMET_065B.freq, pipelines, animal_size, 32),
            label=f"size {animal_size}",
        )

    # / 2 due to off-board RAM hosting requires transfer back and forth to be usable
    ax.axhline(y=COMET_065B.pcie_throughput, label="PCIE Throughput Max")
    ax.yaxis.set_major_formatter(FuncFormatter(bits_to_gb))

    ax.set_title("Effective free memory on host")
    ax.set_xlim(MIN_PIPELINES, MAX_PIPELINES)
    ax.set_xlabel("Pipelines")
    ax.set_ylabel("Generated Data (Gb)")
    ax.legend()

    plt.show()


def memory_for_halo(nr_animals, animal_size, cell_size, halo_steps, halo_step_size):
    """The memory stuck in the time halo waiting for causality resolution"""
    nr_cells = AVERAGE_ANIMAL_PER_CELL / nr_animals

    # TODO: Adapt for the rhomb that the hexgrid is actually on instead of falsely
    # assuming a square
    square_side = int(math.ceil(math.sqrt(nr_cells)))

    return (
        square_side
        * halo_steps
        * halo_step_size
        * (cell_size + AVERAGE_ANIMAL_PER_CELL * animal_size)
    )


def simulation_area(animal_size, cell_size, memory_size):
    """The maximum simulation area for the available memory."""
    simulation_area_ha = memory_size / (
        cell_size + AVERAGE_ANIMAL_PER_CELL * animal_size
    )
    simulation_area_km2 = np.round(simulation_area_ha / 100)
    return simulation_area_km2


def effective_mem_available_on_host(board: Board, pipelines, halo_steps):
    """The usable memory on the host-PC which would not delay the simulation.
    Since the amount of memory available on the FPGA Accelerator card is
    limited, to get a larger simulation, memory on the host-PC connected over
    PCIe could be used. Since the PCIe link throughput is limitied, by letting
    each cell be simulated multiple ticks before being written back to memory
    could much more off- board memory be made viable without delaying the
    simulation. To not break causality, a halo is put around the ticks at a
    different simulation time which is a zone that goes through a reconciliation
    process as other cells catch up in time. Args: board: The FPGA Accelerator
    card specs.

    :param pipelines: The nr of cells (with their contained animals) processed
        every clk cycle
    :param halo_steps: The points at which to evaluate what nr of ticks the
        simulation is to let any cell run ahead of another

    return:
    memory_on_host: The memory available on the host without slowing down
        how fast the simulation processes ticks to wait for off-board memory to arrive
    mem_in_halo: The memory consumed by
        the reconciliation zone between cells at different simulation times
    """
    nr_animals = board.local_memory / DEFAULT_ANIMAL_SIZE
    print(f"{board.name} | nr of animals: {int(nr_animals):.2e}")

    memory_per_s_fpga = memory_per_s(
        board.freq, pipelines, DEFAULT_ANIMAL_SIZE, DEFAULT_CELL_SIZE
    )
    # TODO: Somehow display what the cycle time is
    # print(
    #     f"min cycle time: {cycle_period[0] * 1e3:.2f} ms, "
    #     f"max cycle time {cycle_period[-1] * 1e3:.2f} ms"
    # )

    # TODO: Plot for different number of HALO step sizes
    mem_in_halo = DEFAULT_ANIMAL_SIZE * memory_for_halo(
        nr_animals, DEFAULT_ANIMAL_SIZE, DEFAULT_CELL_SIZE, halo_steps, 6
    )
    memory_on_host = []

    for i, halo_step in enumerate(halo_steps):
        cycle_period = (board.local_memory - mem_in_halo[i]) / memory_per_s_fpga

        memory_on_host.append(board.local_memory * (halo_step + 1) * cycle_period)

    return (memory_on_host, mem_in_halo)


def show_effective_mem_available_on_host(pipelines):
    """Plot the extra simulation capacity available on the host-PC."""
    _, ax = plt.subplots(1, 2)
    (memory_on_host, mem_in_halo) = effective_mem_available_on_host(
        COMET_065B, pipelines, HALO_STEPS
    )

    max_memory_on_host = 0
    for i, element in enumerate(memory_on_host):
        ax[0].plot(pipelines, element, label=f"Halo steps: {HALO_STEPS[i]}")

        max_memory_on_host = max(max_memory_on_host, np.max(memory_on_host))

    ax[1].plot(HALO_STEPS, mem_in_halo)

    ax[0].set_title("Effective free memory on host")
    ax[0].set_xlim(MIN_PIPELINES, MAX_PIPELINES)
    ax[0].set_ylim(0, max_memory_on_host)
    ax[0].set_xlabel("Pipelines")
    ax[0].set_ylabel("Max usable RAM on host (Gb)")
    ax[0].yaxis.set_major_formatter(FuncFormatter(bits_to_gb))
    ax[0].legend()

    ax[1].set_xlim(min(HALO_STEPS), max(HALO_STEPS))
    ax[1].set_ylim(0, max(mem_in_halo))
    ax[1].set_title("Memory used for animals in halo")
    ax[1].set_xlabel("Halo steps")
    ax[1].set_ylabel("Memory used for animals in halo (Kb)")
    ax[1].yaxis.set_major_formatter(
        FuncFormatter(lambda x, pos: f"{int(x / (8 * 1e3)):d}")
    )
    plt.show()


def show_compare_boards(pipelines):
    """Compare maximum simulation size with different hardware accelerator boards"""

    boards = [COMET_065B, DE10, HBM2E_STARTER, MERCURY_A2700, AGILEX7_STARTER]

    max_memory_on_host = 0
    fig = plt.figure()
    gs = fig.add_gridspec(2, 2)
    ax0 = fig.add_subplot(gs[0, 0])
    ax1 = fig.add_subplot(gs[0, 1])
    ax2 = fig.add_subplot(gs[1, :])

    for board in boards:
        (memory_on_host, _) = effective_mem_available_on_host(
            board, pipelines, np.asarray([100.0])
        )
        ax0.plot(pipelines, memory_on_host[0], label=board.name)
        max_memory_on_host = max(max_memory_on_host, np.max(memory_on_host[0]))

        ax1.plot(pipelines, memory_on_host[0] / board.local_memory, label=board.name)

        area_only_board_mem = simulation_area(
            DEFAULT_ANIMAL_SIZE, DEFAULT_CELL_SIZE, board.local_memory
        )
        ax2.scatter(
            board.local_memory, area_only_board_mem, marker=board.plot_symbol, s=100.0
        )
        ax2.text(
            board.local_memory * 1.08,
            area_only_board_mem * 0.95,
            board.name,
            va="top",
            ha="left",
        )

        middle_point = int(round(len(memory_on_host[0]) / 2))
        mem_w_host = board.local_memory + memory_on_host[0][middle_point]
        area_w_host_mem = simulation_area(
            DEFAULT_ANIMAL_SIZE, DEFAULT_CELL_SIZE, mem_w_host
        )
        ax2.scatter(mem_w_host, area_w_host_mem, marker=board.plot_symbol, s=100.0)
        ax2.text(
            mem_w_host * 1.08,
            area_w_host_mem * 0.95,
            board.name,
            va="top",
            ha="left",
        )

    MAX_MEMORY = 1000
    memory = 8e9 * np.arange(0, MAX_MEMORY)
    simulation_area_km2 = simulation_area(
        DEFAULT_ANIMAL_SIZE, DEFAULT_CELL_SIZE, memory
    )
    ax2.loglog(memory, simulation_area_km2, color="red")

    ax0.set_title("Effective free memory on host")
    ax0.set_xlim(MIN_PIPELINES, MAX_PIPELINES)
    ax0.set_ylim(0, max_memory_on_host)
    ax0.set_xlabel("Pipelines")
    ax0.set_ylabel("Max usable RAM on host (Gb)")
    ax0.yaxis.set_major_formatter(FuncFormatter(bits_to_gb))
    ax0.legend()

    ax1.set_title("Free memory on host vs dev-board")
    ax1.set_xlim(MIN_PIPELINES, MAX_PIPELINES)
    ax1.set_ylim(0, 2)
    ax1.set_xlabel("Pipelines")
    ax1.set_ylabel("Max usable RAM on host (Gb)")
    ax1.legend()

    ax2.axhline(y=EARTH_AREA_KM2, label="Earth", color="blue")
    ax2.axhline(y=ASIA_AREA_KM2, label="Asia", color="red")
    ax2.axhline(y=EUROPE_AREA_KM2, label="Europe", color="green")
    ax2.axhline(y=SWEDEN_AREA_KM2, label="Sweden", color="yellow")
    ax2.set_title("Simulation size for the available memory")
    ax2.set_xlim(1 * 8e9, 8.0e9 * MAX_MEMORY)
    ax2.set_ylim(
        GOTLAND_AREA_KM2, max(np.max(simulation_area_km2), 1.25 * EUROPE_AREA_KM2)
    )
    ax2.xaxis.set_major_formatter(FuncFormatter(bits_to_gb))
    ax2.set_xlabel("Memory (Gb)")
    ax2.set_ylabel("Simulation area (km2)")
    ax2.legend()

    plt.show()


def main():

    parser = argparse.ArgumentParser(
        description="Calculate RAM bottleneck to see if RAM on host PC might help"
    )
    parser.add_argument(
        "plot",
        help=(
            "Plot to show: Throughput (FPGA memory throughput vs animal size), "
            "Effective (Usable host memory without necessitating waiting), or Compare "
            "(compare dev-boards)"
        ),
    )
    args = parser.parse_args()

    pipelines = np.linspace(MIN_PIPELINES, MAX_PIPELINES)
    animal_sizes = 2 ** np.arange(5, 9)

    match args.plot:
        case "Throughput":
            show_fpga_througput(pipelines, animal_sizes)
        case "Effective":
            show_effective_mem_available_on_host(pipelines)
        case "Compare":
            show_compare_boards(pipelines)


def bits_to_gb(x, pos):
    return f"{x / (8 * 1e9):.2f}"


if __name__ == "__main__":
    main()
