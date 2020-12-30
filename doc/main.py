import numpy as np
import matplotlib.pyplot as plt
import matplotlib.axes._axes as axes
import csv


class TreeData:
    def __init__(self, path):
        self.all = TreeData.generate_data(path)
        self.x = self.all[0]
        self.insert = self.all[1]
        self.delete = self.all[2]
        self.find = self.all[3]
        self.equal = self.all[4]
        self.isBT = self.all[5]

    @staticmethod
    def generate_data(path: str):
        with open(path, 'r') as csvfile:
            reader = csv.reader(csvfile, delimiter=';')
            data = list(reader)

        data = list(
            map(list, zip(*data)))  # short circuits at shortest nested list if table is jagged
        return data


def generate_plot(data, xla, yla, titl, descr, legend=['default']):
    fig = plt.figure()
    ax1 = fig.add_axes((0.1, 0.2, 0.8, 0.7))
    assert isinstance(ax1, axes.Axes)
    ax1.ticklabel_format(axis='both', style='sci')

    defaultlegend = [];
    for i in range(1, len(data)):
        defaultlegend.append(data[i][0])
        ax1.scatter(np.array(data[0][1:], int), np.array(data[i][1:], int), s=1)
    # ax1.axhline(0, color='lightgrey', lw=1)
    # plt.axvline(0, color='lightgrey', lw=1)
    if legend[0] == 'default':
        ax1.legend(defaultlegend)
    else:
        ax1.legend(legend)
    ax1.autoscale(enable=True, axis='both')
    plt.title(titl)
    plt.xlabel(xla)
    plt.ylabel(yla)
    plt.figtext(0.5, 0.06, descr, ha="center",  # fontsize=12,
                bbox={"facecolor": "lightgrey", "alpha": 0.5, "pad": 5})

    # font = {'family': 'normal',
    #         'weight': 'normal',
    #         'size': 11}

    # plt.rc('font', **font)
    # fig.set_size_inches(7, 8, forward=True)
    # fig.show()
    # ax1.clf()
    return fig


#####################################################
# GRAPHS ############################################
avl_ab = TreeData('zeitmessung/AVL_ab.csv')
avl_auf = TreeData('zeitmessung/AVL_auf.csv')
avl_rand = TreeData('zeitmessung/AVL_rand.csv')
avl_ab_el = TreeData('zeitmessung/AVL_ab_el.csv')
avl_auf_el = TreeData('zeitmessung/AVL_auf_el.csv')
avl_rand_el = TreeData('zeitmessung/AVL_rand_el.csv')
assert (avl_rand.x == avl_ab.x == avl_auf.x)
x = avl_rand.x

xl = 'Elements'
yl = 'Time / ms'
ylns = 'Time / ns'
avlFolder = 'img/python/avl/'

#####################################################
# ABSTEIGEND ########################################
desc = 'Absteigend, Mittlung über 10'

figure = generate_plot(avl_ab.all, xl, yl, '', desc)
figure.savefig(avlFolder + 'ab.pdf', bbox_inches='tight')
figure = generate_plot(avl_ab_el.all, xl, ylns, '', desc)
figure.savefig(avlFolder + 'ab_el.pdf', bbox_inches='tight')

figure = generate_plot([x, avl_ab.insert, avl_ab.delete], xl, yl, '', desc)
figure.savefig(avlFolder + 'ab_insert_vs_delete.pdf', bbox_inches='tight')

figure = generate_plot([x, avl_ab.insert, avl_ab.delete], xl, yl, '', desc)
figure.savefig(avlFolder + 'ab_insert_vs_delete.pdf', bbox_inches='tight')

#####################################################
# RANDOM ############################################
desc = 'Random, Mittlung über 10'

figure = generate_plot(avl_rand.all, xl, yl, '', desc)
figure.savefig(avlFolder + 'rand.pdf', bbox_inches='tight')
figure = generate_plot(avl_rand_el.all, xl, ylns, '', desc)
figure.savefig(avlFolder + 'rand_el.pdf', bbox_inches='tight')

#####################################################
# AUFSTEIGEND #######################################
desc = 'Aufsteigend, Mittlung über 10'

figure = generate_plot(avl_rand.all, xl, yl, '', desc)
figure.savefig(avlFolder + 'auf_el.pdf', bbox_inches='tight')
figure = generate_plot(avl_rand.all, xl, ylns, '', desc)
figure.savefig(avlFolder + 'auf_el.pdf', bbox_inches='tight')

#####################################################
# ANDERE ############################################
desc = 'Insert, Mittlung über 10'

figure = generate_plot([x, avl_ab.insert, avl_auf.insert, avl_rand.insert], xl, yl, '', desc,
                       ['absteigend', 'aufsteigend', 'zufällig'])
figure.savefig(avlFolder + 'insert.pdf', bbox_inches='tight')
