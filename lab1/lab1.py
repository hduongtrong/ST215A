def plot_voltage_epoch():
    figure()
    for nodeid in unique(net[" nodeid"]):
        plot(net[" epoch"][net[" nodeid"] == nodeid], net[" voltage"][net[" nodeid"] == nodeid])


df = pd.merge(net, log, how = "inner", on = [" epoch", " nodeid"])
#df = pd.merge(net, log, how = "left" , on = [" epoch", " nodeid"])

def f(nodeid):
    figure()
    
    indexN = net[" nodeid"] == nodeid
    indexL = log[" nodeid"] == nodeid 
    
    subplot(221)
    plot(log[" epoch"][indexL], log[" humidity"][indexL])
    plot(net[" epoch"][indexN], 1.2*net[" humidity"][indexN])
    title("Humid")
    
    subplot(222)
    plot(log[" epoch"][indexL], log[" humid_temp"][indexL])
    plot(net[" epoch"][indexN], 1.2*net[" humid_temp"][indexN])
    title("Temperature")

    subplot(223)
    plot(log[" epoch"][indexL], log[" hamatop"][indexL])
    plot(net[" epoch"][indexN], 1.2*net[" hamatop"][indexN])
    title("Top")

    subplot(224)
    plot(log[" epoch"][indexL], log[" hamabot"][indexL])
    plot(net[" epoch"][indexN], 1.2*net[" hamabot"][indexN])
    title("Bottom")