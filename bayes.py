import pysmile

# pysmile_license is your license key

import pysmile_license

class MineSweeper:
    def __init__(self, width, height):
        net = pysmile.Network()
        
        # net.read_file("MineSweeper.xdsl")
        self.width = width
        self.height = height
        
        self.net = net
      
        self.create_fields(net)
        self.create_field_values(net)

    def save_network(self):
        self.net.write_file("MineSweeper.xdsl")
    # creating net #
    def create_fields(self, net):
        cm_eq = "Count_Mines="
        fields = []
        for i in range(1, self.width + 1):
            for j in range(1, self.height + 1):
                f = self.create_cpt_node(net,
                        "Field_{}_{}".format(j,i), 
                        "Field({}, {})".format(j,i),
                        ["Field","Mine"], 
                        70 * i, 70 * j,
                        60, 60)
                fields.append(f)
                cm_eq += "Field_{}_{}+".format(j,i)
        cm_eq = cm_eq[0:-1]
        cm_disc = []
        for i in range(self.width * self.height + 1):
            cm_disc.append(pysmile.DiscretizationInterval("V{}".format(i), i + 1.0))
        cm = self.create_eq_node(net,
                "Count_Mines",
                "Count Mines",
                cm_eq,
                cm_disc,
                [0, self.width * self.height + 0.01],
                70, 70 * (1 + self.height+1),
                100, 100)
                
    def create_field_values(self, net):
        for i in range(1, self.width + 1):
            for j in range(1, self.height + 1):                
                neighbours = 0
                eq = "Field_{}_{}_Value=If(Field_{}_{}=0,".format(j,i,j,i)
                for offset_y in [-1,0,1]:
                    for offset_x in [-1,0,1]:
                        if (offset_x != 0 or offset_y != 0) and i + offset_x > 0 and i + offset_x <= self.width and j + offset_y > 0 and j + offset_y <= self.height:
                            eq += "Field_{}_{}+".format(j+offset_y, i+offset_x)
                            neighbours += 1
                eq = eq[0:-1]
                eq += ",{}.01)".format(neighbours+1)
                
                disc = []
                for k in range(neighbours+1):
                    disc.append(pysmile.DiscretizationInterval("V{}".format(k), k + 1.0))
                disc.append(pysmile.DiscretizationInterval("Mine", neighbours + 1.1))
            
                f = self.create_eq_node(net,
                        "Field_{}_{}_Value".format(j,i), 
                        "Field({}, {}) Value".format(j,i),
                        eq,
                        disc,
                        [0, neighbours + 1.01],
                        70 * (1 + self.width + i), 70 * j,
                        60, 60)

    def create_eq_node(self, net, id, name, equation, discretization, bounds, x_pos, y_pos, x_size, y_size):
        handle = net.add_node(pysmile.NodeType.EQUATION, id)
        net.set_node_name(handle, name)
        net.set_node_position(handle, x_pos, y_pos, x_size, y_size)
        net.set_node_equation(handle, equation)
        net.set_node_equation_bounds(handle, bounds[0], bounds[1])
        net.set_node_equation_discretization(handle, discretization)
        return handle
        
    def create_cpt_node(self, net, id, name, outcomes, x_pos, y_pos, x_size, y_size):
        handle = net.add_node(pysmile.NodeType.CPT, id)
        net.set_node_name(handle, name)
        net.set_node_position(handle, x_pos, y_pos, x_size, y_size)
        initial_outcome_count = net.get_outcome_count(handle)
        for i in range(0, initial_outcome_count):
            net.set_outcome_id(handle, i, outcomes[i])
        for i in range(initial_outcome_count, len(outcomes)):
            net.add_outcome(handle, outcomes[i])
        return handle
    # end creating net # 
    
    # functions to call #
    # ustawia liczbę min w sieci
    def set_mine_count(self, mines):
        net = self.net
        net.set_cont_evidence("Count_Mines", mines)
    
    # ustawia wartość pola o podanych współrzędnych na podaną wartość 
    # value = liczba min, lub "M" gdy mina
    def set_field(self, field_x, field_y, value):
        net = self.net
        handler = net.get_node("Field_{}_{}_Value".format(field_x,field_y))
        
        if value == "M":
            value = net.get_node_equation_bounds("Field_{}_{}_Value".format(field_x,field_y))[1]

        net.set_cont_evidence("Field_{}_{}_Value".format(field_x,field_y), value)
    
    # przelicza prawdopodobieństwa w sieci
    # musi być wywołane zawsze gdy pobiera się wiedzę, a od ostatniego przeliczenia sieć była modyfikowana
    def update_knowledge(self):
        self.net.update_beliefs()
    
    # pobiera prawdopodobieństwa w danym polu
    # zwracana lista [Pole,Mina], czyli [0] = szansa na pole, [1] = szansa na minę
    def get_field(self, field_x, field_y):
        beliefs = self.net.get_node_value("Field_{}_{}".format(field_x,field_y))
        knowledge = []
        for i in range(0, len(beliefs)):
            print(self.net.get_outcome_id("Field_{}_{}".format(field_x,field_y), i) + "=" + str(beliefs[i]))
            knowledge.append(beliefs[i])
        return knowledge
    
    # pobiera wszystkie prawdopodobieństwa w sieci
    # zwracana jest liczba jednowymiarowa, więc pole x,y to knowledge[(x - 1) * height + (y - 1)]
    def get_knowledge(self):
        knowledge = []
        for i in range(1,self.width + 1):
            for j in range(1,self.height + 1):
                knowledge.append([])
                beliefs = self.net.get_node_value("Field_{}_{}".format(j,i))
                print("Field_{}_{}:".format(j,i))
                for k in range(0, len(beliefs)):
                    print(self.net.get_outcome_id("Field_{}_{}".format(j,i), k) + "=" + str(beliefs[k]))
                    knowledge[(i - 1) * self.height + (j - 1)].append(beliefs[k])
        return knowledge
    # end functions to call #
    
# przykład użycia
#ms = MineSweeper(3,3)
#ms.save_network() # zapis sieci do pliku
#ms.set_mine_count(5)
#ms.set_field(1,1,3.0)
#ms.update_knowledge()
#field = ms.get_field(1,2)
#print(field)
#knowledge = ms.get_knowledge()
#print(knowledge)
#print(knowledge[5])