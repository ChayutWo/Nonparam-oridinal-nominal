from missingdata_loader import *

def test_loader():
    missing_data_name = 'MCAR_PUMS_test'
    data_name = 'PUMS_test'
    data_x, miss_data_x, data_m = data_loader(missing_data_name, data_name)
    print('fist five rows')
    print(miss_data_x[0:5,:])
    print('masked matrix')
    print(data_m[0:5,:])
    print('array shape')
    print(miss_data_x.shape)
    print('object type')
    print(type(miss_data_x))
    return

test_loader()